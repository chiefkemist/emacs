/* Native, read-only macOS keyboard-layout access for Emacs.
   No keyboard settings are changed and no key events are monitored. */

#include <emacs-module.h>
#include <Carbon/Carbon.h>
#include <CoreFoundation/CoreFoundation.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define EXPORT __attribute__((visibility("default")))
#define CHECK_EXIT(env, result)                                      \
  do {                                                              \
    if ((env)->non_local_exit_check(env) != emacs_funcall_exit_return) \
      return (result);                                              \
  } while (0)

EXPORT int plugin_is_GPL_compatible;

enum { key_variants = 128 * 2 };

struct key_mapping
{
  uint32_t character;
  uint32_t base;
};

static bool
translate(const UCKeyboardLayout *layout, UInt16 key, UInt32 modifiers,
          UInt32 keyboard_type, uint32_t *character)
{
  UInt32 dead = 0;
  UniCharCount length = 0;
  UniChar text[8] = {0};
  OSStatus status = UCKeyTranslate(layout, key, kUCKeyActionDown, modifiers,
                                   keyboard_type, 0, &dead, 8, &length, text);
  if (status != noErr || dead != 0)
    return false;
  if (length == 1 && !(text[0] >= 0xD800 && text[0] <= 0xDFFF))
    *character = text[0];
  else if (length == 2 && text[0] >= 0xD800 && text[0] <= 0xDBFF
           && text[1] >= 0xDC00 && text[1] <= 0xDFFF)
    *character = 0x10000u + ((text[0] - 0xD800u) << 10) + (text[1] - 0xDC00u);
  else
    return false;
  return true;
}

static bool
contains(const uint32_t *characters, size_t count, uint32_t character)
{
  for (size_t i = 0; i < count; ++i)
    if (characters[i] == character)
      return true;
  return false;
}

static int
compare_mapping(const void *left, const void *right)
{
  const struct key_mapping *a = left;
  const struct key_mapping *b = right;
  return (a->character > b->character) - (a->character < b->character);
}

static emacs_value
layout_error(emacs_env *env, emacs_value Qnil, const char *message)
{
  emacs_value error = env->intern(env, "error");
  CHECK_EXIT(env, Qnil);
  emacs_value text = env->make_string(env, message, (ptrdiff_t)strlen(message));
  CHECK_EXIT(env, Qnil);
  env->funcall(env, error, 1, &text);
  return Qnil;
}

static emacs_value
read_option_layout(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void)nargs;
  (void)args;
  (void)data;
  emacs_value Qnil = env->intern(env, "nil");
  CHECK_EXIT(env, Qnil);

  TISInputSourceRef source = TISCopyCurrentKeyboardLayoutInputSource();
  if (source == NULL)
    return layout_error(env, Qnil, "macOS did not provide a keyboard layout");
  CFDataRef layout_data = TISGetInputSourceProperty(source, kTISPropertyUnicodeKeyLayoutData);
  CFStringRef identifier = TISGetInputSourceProperty(source, kTISPropertyInputSourceID);
  char layout_id[1024];
  if (layout_data == NULL || CFGetTypeID(layout_data) != CFDataGetTypeID()
      || CFDataGetLength(layout_data) < (CFIndex)sizeof(UCKeyboardLayout)
      || identifier == NULL || CFGetTypeID(identifier) != CFStringGetTypeID()
      || !CFStringGetCString(identifier, layout_id, (CFIndex)sizeof(layout_id),
                             kCFStringEncodingUTF8))
    {
      CFRelease(source);
      return layout_error(env, Qnil, "macOS keyboard layout has no usable Unicode key data");
    }

  const UCKeyboardLayout *layout = (const UCKeyboardLayout *)CFDataGetBytePtr(layout_data);
  UInt32 keyboard_type = LMGetKbdType();
  uint32_t ordinary[key_variants];
  struct key_mapping candidates[key_variants];
  size_t ordinary_count = 0, candidate_count = 0;
  for (UInt16 key = 0; key < 128; ++key)
    for (unsigned shift = 0; shift < 2; ++shift)
      {
        UInt32 modifiers = shift ? (shiftKey >> 8) : 0;
        uint32_t plain, option;
        if (!translate(layout, key, modifiers, keyboard_type, &plain))
          continue;
        ordinary[ordinary_count++] = plain;
        if (plain >= 32 && plain <= 126
            && translate(layout, key, modifiers | (optionKey >> 8), keyboard_type, &option)
            && option > 127)
          candidates[candidate_count++] = (struct key_mapping){option, plain};
      }
  /* All CF-owned values are released before any fallible Lisp allocation. */
  CFRelease(source);

  qsort(candidates, candidate_count, sizeof(candidates[0]), compare_mapping);
  emacs_value list = env->intern(env, "list");
  CHECK_EXIT(env, Qnil);
  emacs_value cons = env->intern(env, "cons");
  CHECK_EXIT(env, Qnil);
  emacs_value keys = Qnil;
  /* Walk groups backwards so cons produces the same ascending map each time. */
  for (size_t end = candidate_count; end > 0;)
    {
      size_t start = end - 1;
      uint32_t character = candidates[start].character;
      while (start > 0 && candidates[start - 1].character == character)
        --start;
      bool ambiguous = false;
      for (size_t i = start + 1; i < end; ++i)
        if (candidates[i].base != candidates[start].base)
          ambiguous = true;
      if (!ambiguous && !contains(ordinary, ordinary_count, character))
        {
          emacs_value pair_args[2];
          pair_args[0] = env->make_integer(env, character);
          CHECK_EXIT(env, Qnil);
          pair_args[1] = env->make_integer(env, candidates[start].base);
          CHECK_EXIT(env, Qnil);
          emacs_value pair = env->funcall(env, list, 2, pair_args);
          CHECK_EXIT(env, Qnil);
          emacs_value key_args[] = {pair, keys};
          keys = env->funcall(env, cons, 2, key_args);
          CHECK_EXIT(env, Qnil);
        }
      end = start;
    }

  emacs_value entry_args[2], entries[2];
  entry_args[0] = env->intern(env, "layout");
  CHECK_EXIT(env, Qnil);
  entry_args[1] = env->make_string(env, layout_id, (ptrdiff_t)strlen(layout_id));
  CHECK_EXIT(env, Qnil);
  entries[0] = env->funcall(env, cons, 2, entry_args);
  CHECK_EXIT(env, Qnil);
  entry_args[0] = env->intern(env, "keys");
  CHECK_EXIT(env, Qnil);
  entry_args[1] = keys;
  entries[1] = env->funcall(env, cons, 2, entry_args);
  CHECK_EXIT(env, Qnil);
  return env->funcall(env, list, 2, entries);
}

EXPORT int
emacs_module_init(struct emacs_runtime *runtime)
{
  if (runtime->size < (ptrdiff_t)sizeof(*runtime))
    return 1;
  emacs_env *env = runtime->get_environment(runtime);
  /* Only the stable Emacs 25 API prefix is used. */
  if (env->size < (ptrdiff_t)sizeof(struct emacs_env_25))
    return 2;

  emacs_value symbol = env->intern(env, "chief--macos-option-layout");
  CHECK_EXIT(env, 3);
  emacs_value function = env->make_function(
    env, 0, 0, read_option_layout,
    "Return the native macOS layout and unambiguous printable Option-key mappings.", NULL);
  CHECK_EXIT(env, 3);
  emacs_value fset = env->intern(env, "fset");
  CHECK_EXIT(env, 3);
  emacs_value args[] = {symbol, function};
  env->funcall(env, fset, 2, args);
  CHECK_EXIT(env, 3);
  emacs_value feature = env->intern(env, "chief-macos-option-keys");
  CHECK_EXIT(env, 3);
  emacs_value provide = env->intern(env, "provide");
  CHECK_EXIT(env, 3);
  env->funcall(env, provide, 1, &feature);
  CHECK_EXIT(env, 3);
  return 0;
}
