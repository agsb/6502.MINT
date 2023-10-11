# 6502.MINT

A still raw [Mint](https://github.com/monsonite/MINT) for 6502.

## Parameters

  - depends on UART for, _hitc_, check if a key pressed, _getc_, wait and receive a byte, _putc_, wait and transmit a byte
  - uses RAM for fixed stacks (128 x 2), user variables (26 x 2) and user functions (26 x 2).
  - Stacks are round-robin using absolute address and word offsets in X and Y
  - uses a list of byte offsets for functions (as in Z80)
  - user functions mapped as sequences of strings null termined (no : or ;)
  - no garbage colector for user functions, when redefined, the old contents is lost at memory
  - interpreter uses spaces as separator, or none.

## Run Time

