$(document).on("click", "button.groupedButtons", function(evt) {
  // evt.target is the button that was clicked
  var el = $(evt.target);

  // set parents attribute 'data-clicked' to the index of the clicked button
  el.parent().parent().parent().parent().attr('data-clicked',el.attr('data-name'));
  el.parent().parent().parent().parent().attr('data-value',el.attr('data-value'));
  el.parent().parent().parent().parent().trigger("change");
});

$(document).on("change", "div.groupedButtons-parent", function(evt) {
  // evt.target is the button that was clicked
  var el = $(evt.target);

  // set parents attribute 'data-clicked' to the index of the clicked button
  el.children().children().children().children().removeClass('btn-primary');
  el.children().children().children().children('#' + el.attr('data-name') + '-' + el.attr('data-value')).addClass('btn-primary');
});

var groupedButtonsBinding = new Shiny.InputBinding();
$.extend(groupedButtonsBinding, {
  find: function(scope) {
    return $(scope).find(".groupedButtons-parent");
  },
  getValue: function(el) {
    return $(el).attr('data-value');
  },
  setValue: function(el, value) {
    $(el).attr('data-clicked',value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.groupedButtonsBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".groupedButtonsBinding");
  }
});
Shiny.inputBindings.register(groupedButtonsBinding);


Shiny.addCustomMessageHandler('updateGroupedActionButton', function(message) {

  var element = $("#" + message.element);
  var value  = message.value;
  var selected = message.selected;

  element.attr('data-clicked',selected);
  element.attr('data-value',value);
  element.trigger("change");
});
