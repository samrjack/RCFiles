# This file has been generated using org tangle. To modify, please see the org file.

config.load_autoconfig()

c.auto_save.session = True

c.scrolling.smooth = True

c.tabs.width = "10%"

c.content.pdfjs = True
c.content.plugins = True
c.content.cache.size = 2147483647

c.spellcheck.languages = ["en-US"]

#c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"

c.hints.chars = "abcdefghijklmnopqrstuvwxyz"

c.completion.cmd_history_max_items = 10000
c.completion.use_best_match = True

c.downloads.location.suggestion = "both"
c.confirm_quit = ["downloads"]

config.unbind('b')

config.bind('<Ctrl-q>', 'quit')
config.bind('ZQ', 'quit')
config.bind('ZZ', 'quit --save')

# Standard scrolling
config.bind('h', 'scroll left')
config.bind('j', 'scroll down')
config.bind('k', 'scroll up')
config.bind('l', 'scroll right')

# Scrolling up
config.bind('u', 'scroll-page 0 -0.5')
config.bind('U', 'scroll-page 0 -1')
config.bind('<Ctrl-u>', 'scroll-page 0 -0.5')

#Emacs
config.bind('<Ctrl-b>', 'scroll-page 0 -1')

# Scrolling down
config.bind('d', 'scroll-page 0 0.5')
config.bind('D', 'scroll-page 0 1')
config.bind('<Ctrl-d>', 'scroll-page 0 0.5')

# Scrolling top/bottom
config.bind('gg', 'scroll-to-perc 0')
config.bind('G', 'scroll-to-perc')

config.bind('/', 'set-cmd-text /')
config.bind('?', 'set-cmd-text ?')
config.bind('n', 'search-next')
config.bind('N', 'search-prev')

config.bind('o', 'set-cmd-text -s :open')
config.bind('O', 'set-cmd-text -s :open -t')
config.bind('<Ctrl-o>', 'set-cmd-text -s :open -r {url:pretty}')
config.bind('<Alt-o>', 'set-cmd-text -s :open -t -r {url:pretty}')
config.bind('<Ctrl-t>', 'open -t')
config.bind('T', 'set-cmd-text -s :open -t')
config.bind('pp', 'open -- {clipboard}')
config.bind('pP', 'open -- {primary}')
config.bind('Pp', 'open -t -- {clipboard}')
config.bind('PP', 'open -t -- {primary}')
config.bind('<Ctrl-Shift-n>', 'open -p')

config.bind('^', 'tab-focus 1')
config.bind('$', 'tab-focus -1')
config.bind('<Ctrl-^>', 'tab-focus last')

config.bind('J', 'tab-next')
config.bind('K', 'tab-prev')

config.bind('<Ctrl-Tab>', 'tab-focus last')
config.bind('<Ctrl-PgDown>', 'tab-next')
config.bind('<Ctrl-PgUp>', 'tab-prev')

config.bind('<Alt-1>', 'tab-focus 1')
config.bind('<Alt-2>', 'tab-focus 2')
config.bind('<Alt-3>', 'tab-focus 3')
config.bind('<Alt-4>', 'tab-focus 4')
config.bind('<Alt-5>', 'tab-focus 5')
config.bind('<Alt-6>', 'tab-focus 6')
config.bind('<Alt-7>', 'tab-focus 7')
config.bind('<Alt-8>', 'tab-focus 8')
config.bind('<Alt-9>', 'tab-focus 9')
config.bind('<Alt-0>', 'tab-focus -1')

config.bind('<', 'tab-move -')
config.bind('>', 'tab-move +')
config.bind('x', 'tab-close')
config.bind('X', 'undo')
config.bind('<Alt-p>', 'tab-pin')

config.bind('<Ctrl-Shift-t>', 'undo')

# Open previous page
config.bind('H', 'back')
# Go to next page
config.bind('L', 'forward')

# Open next page in a new background tab
config.bind('<Ctrl-l>', 'forward --tab --bg')
# Open prevous page in a new background tab
config.bind('<Ctrl-h>', 'back --tab --bg')

config.bind('th', 'back -t')
config.bind('tl', 'forward -t')
config.bind('<back>', 'back')
config.bind('<forward>', 'forward')

config.bind('f', 'hint all current')
config.bind('F', 'hint all tab-fg')
config.bind('<Ctrl-f>', 'hint all tab-bg')

config.bind('{{', 'navigate prev -t')
config.bind('}}', 'navigate next -t')
config.bind('[[', 'navigate prev')
config.bind(']]', 'navigate next')
config.bind('<Ctrl-a>', 'navigate increment')
config.bind('<Ctrl-x>', 'navigate decrement')

config.bind(',u', 'navigate up')
config.bind(',s', 'navigate strip')
config.bind(',p', 'navigate prev')
config.bind(',n', 'navigate next')
config.bind(',i', 'navigate increment')
config.bind(',d', 'navigate decrement')
config.bind(',U', 'navigate --tab up')
config.bind(',S', 'navigate --tab strip')
config.bind(',P', 'navigate --tab prev')
config.bind(',N', 'navigate --tab next')
config.bind(',I', 'navigate --tab increment')
config.bind(',D', 'navigate --tab decrement')
config.bind('<Ctrl-,>u', 'navigate --tab up')
config.bind('<Ctrl-,>s', 'navigate --tab strip')
config.bind('<Ctrl-,>p', 'navigate --tab prev')
config.bind('<Ctrl-,>n', 'navigate --tab next')
config.bind('<Ctrl-,>i', 'navigate --tab increment')
config.bind('<Ctrl-,>d', 'navigate --tab decrement')
config.bind('<Alt-,>u', 'navigate --bg up')
config.bind('<Alt-,>s', 'navigate --bg strip')
config.bind('<Alt-,>p', 'navigate --bg prev')
config.bind('<Alt-,>n', 'navigate --bg next')
config.bind('<Alt-,>i', 'navigate --bg increment')
config.bind('<Alt-,>d', 'navigate --bg decrement')

config.bind(';b', 'hint all tab-bg')
config.bind(';d', 'hint all download')
config.bind(';D', 'hint all delete')
config.bind(';f', 'hint all tab-fg')
config.bind(';h', 'hint all hover')
config.bind(';i', 'hint images')
config.bind(';I', 'hint images tab')
config.bind(';o', 'hint links fill :open {hint-url}')
config.bind(';O', 'hint links fill :open -t -r {hint-url}')
config.bind(';r', 'hint --rapid links tab-bg')
config.bind(';R', 'hint --rapid links window')
config.bind(';t', 'hint inputs')
config.bind(';y', 'hint links yank')
config.bind(';Y', 'hint links yank-primary')
config.bind('<Alt-d>', 'hint all delete')

config.bind('<Return>', 'selection-follow')
config.bind('<Ctrl-Return>', 'selection-follow -t')

config.bind('yd', 'yank domain')
config.bind('yD', 'yank domain -s')
config.bind('yf', 'hint all yank')
config.bind('yp', 'yank pretty-url')
config.bind('yP', 'yank pretty-url -s')
config.bind('yt', 'yank title')
config.bind('yT', 'yank title -s')
config.bind('yy', 'yank')
config.bind('yY', 'yank -s')

config.bind('=', 'zoom') # resets zoom level to 100%
config.bind('+', 'zoom-in')
config.bind('-', 'zoom-out')

config.bind('bd', 'set-cmd-text -s :quickmark-del ')
config.bind('bl', 'set-cmd-text -s :quickmark-load')
config.bind('bs', 'set-cmd-text -s :quickmark-add {url:pretty} ')
config.bind('B', 'set-cmd-text -s :quickmark-load -t')
config.bind('m', 'quickmark-save')
config.bind('M', 'bookmark-add')

config.bind('cd', 'download-clear')
config.bind('cm', 'clear-messages')
config.bind('co', 'tab-only')
config.bind('ct', 'tab-only')

config.bind('i', 'mode-enter insert')
config.bind('I', 'mode-enter passthrough')
config.bind('v', 'mode-enter caret')
config.bind('<Ctrl-v>', 'mode-enter passthrough')
config.bind('`', 'mode-enter set_mark')
config.bind("'", 'mode-enter jump_mark')
config.bind(':', 'set-cmd-text :')

config.bind('sd', 'set-cmd-text -s :session-delete ')
config.bind('sl', 'set-cmd-text -s :session-load ')
config.bind('sr', 'set-cmd-text -s :session-load ')
config.bind('ss', 'set-cmd-text -s :session-save -o ')
config.bind('sS', 'set-cmd-text -s :session-save -p ')

config.bind('wB', 'set-cmd-text -s :bookmark-load -w')
config.bind('wO', 'set-cmd-text :open -w {url:pretty}')
config.bind('wP', 'open -w -- {primary}')
config.bind('wb', 'set-cmd-text -s :quickmark-load -w')
config.bind('wf', 'hint all window')
config.bind('wh', 'back -w')
config.bind('wi', 'inspector')
config.bind('wl', 'forward -w')
config.bind('wo', 'set-cmd-text -s :open -w')
config.bind('wp', 'open -w -- {clipboard}')
config.bind('<Ctrl-n>', 'open -w')

config.bind('r', 'reload')
config.bind('R', 'reload -f')
config.bind('<F5>', 'reload')
config.bind('<Ctrl-F5>', 'reload -f')
config.bind('<Ctrl-r>', 'config-source')

config.bind('q', 'record-macro')
config.bind('@', 'run-macro')

config.bind('g$', 'tab-focus -1')
config.bind('g0', 'tab-focus 1')
config.bind('gB', 'set-cmd-text -s :bookmark-load -t')
config.bind('gC', 'tab-clone')
config.bind('gO', 'set-cmd-text :open -t -r {url:pretty}')
config.bind('gU', 'navigate up -t')
config.bind('g^', 'tab-focus 1')
config.bind('ga', 'open -t')
config.bind('gb', 'set-cmd-text -s :bookmark-load')
config.bind('gd', 'download')
config.bind('gf', 'view-source')
config.bind('gl', 'tab-move -')
config.bind('gm', 'tab-move')
config.bind('go', 'set-cmd-text :open {url:pretty}')
config.bind('gr', 'tab-move +')
config.bind('gt', 'set-cmd-text -s :buffer')
config.bind('gu', 'navigate up')

config.bind('Sb', 'open qute://bookmarks#bookmarks')
config.bind('Sh', 'open qute://history')
config.bind('Sq', 'open qute://bookmarks')
config.bind('Ss', 'open qute://settings')
config.bind('<Ctrl-m>', 'messages -t')

config.bind('<Ctrl-c>', 'stop')
config.bind('<Ctrl-s>', 'stop')
config.bind('.', 'repeat-command')
config.bind('<F11>', 'fullscreen')
config.bind('<Alt-b>', 'fullscreen')
config.bind('<Ctrl-p>', 'print')

config.bind('$', 'move-to-end-of-line', mode='caret')
config.bind('{', 'move-to-end-of-prev-block', mode='caret')
config.bind('}', 'move-to-end-of-next-block', mode='caret')
config.bind('[', 'move-to-start-of-prev-block', mode='caret')
config.bind(']', 'move-to-start-of-next-block', mode='caret')
config.bind('b', 'move-to-prev-word', mode='caret')
config.bind('c', 'mode-enter normal', mode='caret')
config.bind('e', 'move-to-end-of-word', mode='caret')
config.bind('gg', 'move-to-start-of-document', mode='caret')
config.bind('G', 'move-to-end-of-document', mode='caret')
config.bind('h', 'move-to-prev-char', mode='caret')
config.bind('H', 'scroll left', mode='caret')
config.bind('j', 'move-to-next-line', mode='caret')
config.bind('J', 'scroll down', mode='caret')
config.bind('k', 'move-to-prev-line', mode='caret')
config.bind('K', 'scroll up', mode='caret')
config.bind('l', 'move-to-next-char', mode='caret')
config.bind('L', 'scroll right', mode='caret')
config.bind('v', 'toggle-selection', mode='caret')
config.bind('w', 'move-to-next-word', mode='caret')
config.bind('y', 'yank selection', mode='caret')
config.bind('Y', 'yank selection -s', mode='caret')
config.bind('0', 'move-to-start-of-line', mode='caret')
config.bind('<Ctrl-Space>', 'drop-selection', mode='caret')
config.bind('<Escape>', 'mode-leave', mode='caret')
config.bind('<Return>', 'yank selection', mode='caret')
config.bind('<Space>', 'toggle-selection', mode='caret')

config.bind('<Ctrl-b>', 'rl-backward-char', mode='command')
config.bind('<Alt-b>', 'rl-backward-word', mode='command')
config.bind('<Ctrl-f>', 'rl-forward-char', mode='command')
config.bind('<Alt-f>', 'rl-forward-word', mode='command')
config.bind('<Ctrl-a>', 'rl-beginning-of-line', mode='command')
config.bind('<Ctrl-e>', 'rl-end-of-line', mode='command')

# Deleting
config.bind('<Ctrl-d>', 'rl-delete-char', mode='command')
config.bind('<Alt-d>', 'rl-kill-word', mode='command')
config.bind('<Backspace>', 'rl-backward-delete-char', mode='command')
config.bind('<Alt-Backspace>', 'rl-backward-kill-word', mode='command')
config.bind('<Ctrl-h>', 'rl-backward-delete-char', mode='command')
config.bind('<Ctrl-w>', 'rl-backward-kill-word', mode='command')
config.bind('<Ctrl-k>', 'rl-kill-line', mode='command')
config.bind('<Ctrl-u>', 'rl-unix-line-discard', mode='command')

# yanking
config.bind('<Ctrl-y>', 'rl-yank', mode='command')
config.bind('<Ctrl-h>', 'rl-backward-delete-char', mode='command')
config.bind('<Ctrl-?>', 'rl-delete-char', mode='command')
config.bind('<Ctrl-w>', 'rl-unix-word-rubout', mode='command')

config.bind('<Ctrl-n>', 'command-history-next', mode='command')
config.bind('<Ctrl-p>', 'command-history-prev', mode='command')
config.bind('<Up>', 'completion-item-focus --history prev', mode='command')
config.bind('<Down>', 'completion-item-focus --history next', mode='command')

config.bind('<Shift-Delete>', 'completion-item-del', mode='command')
config.bind('<Ctrl-Shift-c>', 'completion-item-yank --sel', mode='command')
config.bind('<Ctrl-Shift-tab>', 'completion-item-focus prev-category', mode='command')
config.bind('<Ctrl-tab>', 'completion-item-focus next-category', mode='command')
config.bind('<Shift-Tab>', 'completion-item-focus prev', mode='command')
config.bind('<Tab>', 'completion-item-focus next', mode='command')
config.bind('<Return>', 'command-accept', mode='command')
config.bind('<Ctrl-Return>', 'command-accept --rapid', mode='command')
config.bind('<Escape>', 'mode-leave', mode='command')

config.bind('<Ctrl-b>', 'hint all tab-bg', mode='hint')
config.bind('<Ctrl-f>', 'hint links', mode='hint')
config.bind('<Ctrl-r>', 'hint --rapid links tab-bg', mode='hint')
config.bind('<Escape>', 'mode-leave', mode='hint')
config.bind('<Return>', 'follow-hint', mode='hint')

config.bind('<Escape>', 'mode-leave', mode='insert')
config.bind('<Shift-Ins>', 'insert-text {primary}', mode='insert')
config.bind('<Ctrl-r>', 'insert-text {primary}', mode='insert')

# Recreated readline bindings for insert mode
config.bind('<Ctrl-h>', 'fake-key <Backspace>', mode='insert')
config.bind('<Ctrl-a>', 'fake-key <Home>', mode='insert')
config.bind('<Ctrl-e>', 'fake-key <End>', mode='insert')
config.bind('<Ctrl-b>', 'fake-key <Left>', mode='insert')
config.bind('<Alt-b>', 'fake-key <Ctrl-Left>', mode='insert')
config.bind('<Ctrl-f>', 'fake-key <Right>', mode='insert')
config.bind('<Alt-f>', 'fake-key <Ctrl-Right>', mode='insert')
config.bind('<Ctrl-p>', 'fake-key <Up>', mode='insert')
config.bind('<Ctrl-n>', 'fake-key <Down>', mode='insert')
config.bind('<Alt-d>', 'fake-key <Ctrl-Delete>', mode='insert')
config.bind('<Ctrl-d>', 'fake-key <Delete>', mode='insert')
config.bind('<Ctrl-w>', 'fake-key <Ctrl-Backspace>', mode='insert')
config.bind('<Ctrl-u>', 'fake-key <Shift-Home><Delete>', mode='insert')
config.bind('<Ctrl-k>', 'fake-key <Shift-End><Delete>', mode='insert')
config.bind('<Ctrl-x><Ctrl-e>', 'open-editor', mode='insert')

config.bind('<Alt-b>', 'rl-backward-word', mode='prompt')
config.bind('<Alt-Backspace>', 'rl-backward-kill-word', mode='prompt')
config.bind('<Alt-d>', 'rl-kill-word', mode='prompt')
config.bind('<Alt-f>', 'rl-forward-word', mode='prompt')
config.bind('<Ctrl-?>', 'rl-delete-char', mode='prompt')
config.bind('<Ctrl-a>', 'rl-beginning-of-line', mode='prompt')
config.bind('<Ctrl-b>', 'rl-backward-char', mode='prompt')
config.bind('<Ctrl-e>', 'rl-end-of-line', mode='prompt')
config.bind('<Ctrl-f>', 'rl-forward-char', mode='prompt')
config.bind('<Ctrl-h>', 'rl-backward-delete-char', mode='prompt')
config.bind('<Ctrl-k>', 'rl-kill-line', mode='prompt')
config.bind('<Ctrl-u>', 'rl-unix-line-discard', mode='prompt')
config.bind('<Ctrl-w>', 'rl-unix-word-rubout', mode='prompt')
config.bind('<Ctrl-x>', 'prompt-open-download', mode='prompt')
config.bind('<Down>', 'prompt-item-focus next', mode='prompt')
config.bind('<Escape>', 'mode-leave', mode='prompt')
config.bind('<Return>', 'prompt-accept', mode='prompt')
config.bind('<Shift-Tab>', 'prompt-item-focus prev', mode='prompt')
config.bind('<Tab>', 'prompt-item-focus next', mode='prompt')
config.bind('<Up>', 'prompt-item-focus prev', mode='prompt')
config.bind('<Ctrl-n>', 'prompt-accept no', mode='prompt')
config.bind('<Ctrl-y>', 'prompt-accept yes', mode='prompt')

# Bindings for register mode
config.bind('<Escape>', 'mode-leave', mode='register')

from qutebrowser.api import interceptor

    # Youtube adblock
def filter_yt(info: interceptor.Request):
    """Block the given request if necessary."""
    url = info.request_url
    if (url.host() == 'www.youtube.com' and
        url.path() == '/get_video_info' and
            '&adformat=' in url.query()):
        info.block()

interceptor.register(filter_yt)
