# The editor every media/sdk scene opens: Neovim on farm.py, behind clients/ent-lsp.
Require nvim
Require bb
Require pyright-langserver
Set Shell "bash"
Set Theme "Github"
Set FontSize 18
Set Width 1400
Set Height 380
Set Padding 18
Set TypingSpeed 55ms

Hide
Type 'nvim -u "$SDK_GIF_INIT" farm.py' Enter
Sleep 4s
Type "G"
Show
