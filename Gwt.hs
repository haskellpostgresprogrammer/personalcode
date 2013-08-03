module Gwt where

data App = App [Object] [ClickEvent]
data Object = Object Name Type [InitialValue]
type Name = String
type Type = Widget
data InitialValue = InitialValue Name Value
type Value = String
data ClickEvent = ClickEvent [Code]
type Code = String
data Widgets = Widget [Widget]
data Widget = PushButton
  | RadioButton
  | CheckBox
  | DatePicker
  | ToggleButton
  | TextBox
  | PasswordTextBox
  | TextArea
  | Hyperlink
  | ListBox
  | MenuBar
  | Tree
  | SuggestBox
  | RichTextArea
  | Table
  | TabBar
  | DialogBox
  | PopupPanel
  | StackPanel
  | HorizontalPanel
  | VerticalPanel
  | FlowPanel
  | VerticalSplitPanel
  | HorizontalSplitPanel
  | DockPanel
  | TabPanel
  | DisclosurePanel
