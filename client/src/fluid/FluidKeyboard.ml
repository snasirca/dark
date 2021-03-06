open Tc

type browserPlatform =
  | Mac
  | Linux
  | Windows
  | UnknownPlatform

external jsGetBrowserPlatform : unit -> browserPlatform Js.Nullable.t
  = "getBrowserPlatform"
  [@@bs.val] [@@bs.scope "window"]

let getBrowserPlatform () : browserPlatform =
  jsGetBrowserPlatform ()
  |> Js.Nullable.toOption
  |> Option.withDefault ~default:UnknownPlatform


(* Character representation that's partially keyboard and partially ascii
 * characters. All keyboard keys are represented, but may be split into
 * multiple characters (eg 5 and percent are the same button, but it's
 * worthwhile knowing which is which).
 *)

type shortcutHeritage =
  | LegacyShortcut
  | CurrentShortcut
[@@deriving show]

type key =
  | Space
  | Left
  | Right
  | Up
  | Down
  | Shift of side option
  | Ctrl of side option
  | Alt
  | Tab
  | ShiftTab
  | CapsLock
  | Escape
  | Enter
  | ShiftEnter
  | PageUp
  | PageDown
  | GoToStartOfLine of maintainSelection
  | GoToEndOfLine of maintainSelection
  | GoToStartOfWord of maintainSelection
  | GoToEndOfWord of maintainSelection
  | Undo
  | Redo
  | SelectAll
  | CommandPalette of shortcutHeritage
  | Omnibox
  | Unhandled of string
[@@deriving show]

and side =
  | LeftHand
  | RightHand
[@@deriving show]

and maintainSelection =
  | KeepSelection
  | DropSelection
[@@deriving show]

let toName = show_key

let fromKeyboardEvent
    (key : string) (shift : bool) (ctrl : bool) (meta : bool) (alt : bool) : key
    =
  let isMac = getBrowserPlatform () = Mac in
  let osCmdKeyHeld = if isMac then meta else ctrl in
  let maintainSelection = if shift then KeepSelection else DropSelection in
  match key with
  (*************
   * Shortcuts *
   *************)
  | "a" when osCmdKeyHeld ->
      SelectAll
  | "a" when ctrl ->
      GoToStartOfLine maintainSelection
  | "e" when ctrl ->
      GoToEndOfLine maintainSelection
  | "k" when ctrl || meta ->
      Omnibox
  | "y" when (not isMac) && ctrl && not shift ->
      (* CTRL+Y is Windows redo
      but CMD+Y on Mac is the history shortcut in Chrome (since CMD+H is taken for hide)
      See https://support.google.com/chrome/answer/157179?hl=en *)
      Redo
  | ("Z" | "z") when shift && osCmdKeyHeld ->
      Redo
  | ("Z" | "z") when (not shift) && osCmdKeyHeld ->
      Undo
  | "ArrowLeft" when meta ->
      GoToStartOfLine maintainSelection
  | "ArrowLeft" when (isMac && alt) || ctrl ->
      (* Allowing Ctrl on macs because it doesnt override any default mac cursor movements.
       * Default behaivor is desktop switching where the OS swallows the event unless disabled *)
      GoToStartOfWord maintainSelection
  | "ArrowRight" when meta ->
      GoToEndOfLine maintainSelection
  | "ArrowRight" when (isMac && alt) || ctrl ->
      (* Allowing Ctrl on macs because it doesnt override any default mac cursor movements.
       * Default behaivor is desktop switching where the OS swallows the event unless disabled *)
      GoToEndOfWord maintainSelection
  (************
   * Movement *
   ************)
  | "PageUp" ->
      PageUp
  | "PageDown" ->
      PageDown
  | "End" when ctrl ->
      GoToStartOfLine maintainSelection
  | "End" ->
      GoToEndOfLine maintainSelection
  | "Home" when ctrl ->
      GoToEndOfLine maintainSelection
  | "Home" ->
      GoToStartOfLine maintainSelection
  | "ArrowUp" ->
      Up
  | "ArrowDown" ->
      Down
  | "ArrowLeft" ->
      Left
  | "ArrowRight" ->
      Right
  (*************
   * Modifiers *
   *************)
  | "Shift" ->
      Shift None
  | "Ctrl" ->
      Ctrl None
  | "Alt" ->
      Alt
  | "CapsLock" ->
      CapsLock
  (********
   * Misc *
   ********)
  | "Tab" when shift ->
      ShiftTab
  | "Tab" ->
      Tab
  | "Enter" when shift ->
      ShiftEnter
  | "Enter" ->
      Enter
  | "Escape" ->
      Escape
  | " " ->
      Space
  | "s" when ctrl ->
      CommandPalette CurrentShortcut
  | "\\" when ctrl ->
      CommandPalette CurrentShortcut
  (*~*~*~*~ HERE BE DRAGONS ~*~*~*~*
   * alt-x opens command palatte.
   *
   * Since Alt-something is unreliable on different keyboard layouts, We
   * check for Alt-X, Ctrl-s and Ctrl-\ when opening the command palette.
   * 
   *
   * On macOS, is key = '≈', which we have to hack in with bucklescript JS
   * literals because OCaml is terrible.
   *
   * As a bonus, bucklescript doesn't seem to parse this correctly if it's in a
   * match conditional, meaning you still get UTF-8 escape sequences in the JS
   * instead of an unescaped JS literal. So...
   *
   * DON'T DO THIS:
   *   | {js|≈|js} ->
   * or you end up with this in the JS:
   *   case "\xe2\x89\x88" :
   * whereas what you want is this:
   *   case "≈" :
   *
   * An if statement seems to work correctly, so we use that instead.
   *
   * FIXME: This also means it's impossible to type the literal character '≈' in our
   * editor right now, which we probably should fix at some point. This all
   * points to the fact that it may be easier to do shortcuts with Cmd/Ctrl
   * instead of Alt. *)
  | "x" when alt ->
      CommandPalette CurrentShortcut
  | _ when alt && String.length key = 1 ->
      if key = {js|≈|js}
      then CommandPalette CurrentShortcut
      else Unhandled key
  | _ ->
      Unhandled key


type keyEvent =
  { key : key
  ; shiftKey : bool
  ; ctrlKey : bool
  ; altKey : bool
  ; metaKey : bool }
[@@deriving show]

(** eventToKeyEvent converts the JS KeyboardEvent [evt] into a [keyEvent].
 * Returns (Some keyEvent) or None if a decoding error occurs. *)
let eventToKeyEvent (evt : Web.Node.event) : keyEvent option =
  let open Tea.Json.Decoder in
  let decoder =
    map5
      (fun rawKey shiftKey ctrlKey altKey metaKey ->
        let key = fromKeyboardEvent rawKey shiftKey ctrlKey metaKey altKey in
        {key; shiftKey; ctrlKey; altKey; metaKey})
      (field "key" string)
      (field "shiftKey" bool)
      (field "ctrlKey" bool)
      (field "altKey" bool)
      (field "metaKey" bool)
  in
  decodeEvent decoder evt |> Tea_result.result_to_option


(** onKeydown converts the JS KeyboardEvent [evt] into a keyEvent, then
  * calls the [tagger] with it if successful.
  *
  * [tagger] is a (keyEvent -> Types.msg). It would be nice to simply return
  * the msg option here, but we cannot reference Types here otherwise we get
  * a dependency cycle. *)
let onKeydown tagger (evt : Web.Node.event) =
  evt
  |> eventToKeyEvent
  |> Option.andThen ~f:(function
         | {key = Unhandled _; _} ->
             None
         | kevt ->
             (* if we are going to handle the key, then preventDefault *)
             evt##preventDefault () ;
             Some (tagger kevt))
