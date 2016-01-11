// STYLE_PREFIX.DCL - Dialog to choose text type (prefix)
// by Ken Krupa, Krupa CADD Solutions
// Copyright© 2010 Robert A.M. Stern Architects LLP

dcl_settings : default_dcl_settings { audit_level = 3; }

//////////////////////////////////////////////////////////////////////////////
// WIDGET
text_line : text_part {width = 32;}

//////////////////////////////////////////////////////////////////////////////
// MAIN DIALOGUE
style_prefix : dialog {
  label = " Choose Text Style Type";
  spacer;
  : boxed_column {
    label = "Choice required";
    : paragraph {
      : text_line {value = "The current text style name does not ";}
      : text_line {value = "match any of the approved style types.";}
      spacer;
      : text_line {value = "Please choose the style type to use.";}
    }
  }
  : boxed_radio_column {
    label = "Text style types";
    key = "prefix";
    : radio_button {
      key = "Sans";
      label = "&Sans";
    }
    : radio_button {
      key = "Serif";
      label = "Seri&f";
    }
    : radio_button {
      key = "Hand";
      label = "&Hand";
    }
  }
  ok_cancel;
}


