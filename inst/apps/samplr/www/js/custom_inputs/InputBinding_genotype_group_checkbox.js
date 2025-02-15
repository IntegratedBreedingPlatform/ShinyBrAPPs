var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".genotypes_groups");
  },

  getValue: function(el) {
    var checkedIds = [];
    $(el).find(".group_checkbox").each(function(){
      if ($(this).prop("checked") == true) {
        checkedIds.push($(this).attr("group_id"));
      }
    });
    return checkedIds;
  },

  subscribe: function(el, callback) {
    $(el).on('click', function(event) {
      callback();
    });
  }

})

Shiny.inputBindings.register(binding);
