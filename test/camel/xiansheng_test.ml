open Xiansheng
open Coconuts

TEST_MODULE "offer_help" = struct
  TEST = offer_help (Some (HELP, [EMPTY], ["something"])) = None
  TEST = offer_help (Some (HELP, [CMD], ["branch"])) = None
  TEST = offer_help (Some (HELP, [CMD], ["not_a_cmd"])) = None
  TEST = offer_help (Some (ADD, [ALL], [])) = Some (ADD, [ALL], [])
end