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
    // single groupe selection change
    $(el).on('change.group_checkbox', '.group_checkbox', function(event) {
      callback();
      
      // Check/uncheck cluster 
      var cl_id = $(this).data('clustering-id');
      if (cl_id !== 'manual') {
        var $all = $(el).find('.group_checkbox[data-clustering-id="' + cl_id + '"]');
        var $cluster = $(el).find('.group-cluster-select-all[data-clustering-id="' + cl_id + '"]');
        var allChecked = $all.length > 0 && $all.filter(':checked').length === $all.length;
        $cluster.prop('checked', allChecked);
      }
      
      // Check/uncheck "Select all"
      var $all = $(el).find('.group_checkbox');
      const allGroupsChecked = $all.length > 0 && $all.filter(':checked').length === $all.length;
      $(el).find('.group-global-select-all').prop('checked', allGroupsChecked);;
    });

    // cluster selection change
    $(el).on('change.group_checkbox', '.group-cluster-select-all', function(event) {
      var cl_id = $(this).data('clustering-id');
      var checked = $(this).is(':checked');

      // Check/uncheck manually the children groups
      $(el).find('.group_checkbox[data-clustering-id="' + cl_id + '"]').each(function() {
        $(this).prop('checked', checked);
      });
  
      callback();
    });

    // Select all
    $(el).on('change.group_checkbox', '.group-global-select-all', function(event) {
      var checked = $(this).is(':checked');
  
      $(el).find('.group_checkbox').each(function() {
        $(this).prop('checked', checked);
      });
      $(el).find('.group-cluster-select-all').prop('checked', checked);
  
      callback();
    });
  }

})

Shiny.inputBindings.register(binding);
