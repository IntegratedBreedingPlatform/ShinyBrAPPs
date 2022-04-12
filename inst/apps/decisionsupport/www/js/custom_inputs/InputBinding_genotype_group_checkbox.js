var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".my_selector");
  },

  getValue: function(el) {
    var checkIds = [];
    $(el).find(".group_checkbox").each(function(){
      if ($(this).prop("checked") == true) {
        checkIds.push($(this).attr("group_id"));
      }
    });
    return checkIds;
  },

  subscribe: function(el, callback) {
    $(el).on('click', function(event) {
      callback();
    });
  }

})

Shiny.inputBindings.register(binding);
