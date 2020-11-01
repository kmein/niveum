{
  triggers = {
    night = {
      platform = "numeric_state";
      entity_id = "sun.sun";
      value_template = "{{ state.attributes.elevation }}";
      below = -4.0;
    };
    day = {
      platform = "numeric_state";
      entity_id = "sun.sun";
      value_template = "{{ state.attributes.elevation }}";
      above = 0;
    };
  };
}
