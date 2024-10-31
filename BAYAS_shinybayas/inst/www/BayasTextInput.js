var bayasTextInputBinding = new Shiny.InputBinding();
$.extend(bayasTextInputBinding, {
    name: "shiny.textInput",

    find: function(scope) {
      return $(scope).find('.bayasTextInput');
    },
    getValue: function(el) {
      return $(el).val();
    },
    setValue: function(el, value) {
      $(el).val(value);
    },
    subscribe: function(el, callback) {
      $(el).on('keyup.bayasTextInputBinding input.bayasTextInputBinding', function(event) {
        if(event.keyCode == 13) {
          callback();
        }
      });
    $(el).on('focusout.bayasTextInputBinding', function(event) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.bayasTextInputBinding');
    }
});
Shiny.inputBindings.register(bayasTextInputBinding);

Shiny.addCustomMessageHandler('updateBayasTextInput', function(message) {

  var element = $("#" + message.element);
  var value  = message.value;
  var placeholder = message.placeholder;
  var label = message.label;
  var trigger = message.trigger;

  element.val(value);
  if(placeholder != null) element.attr('placeholder', placeholder);
  if(label != null) {
    var elementLabel = $("#" + message.element + "-label");
    elementLabel.text(label);
  }

  if(trigger) element.trigger("focusout");
});
