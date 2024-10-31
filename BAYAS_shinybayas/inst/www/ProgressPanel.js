$(document).on("click", "button.close-progress", function(evt) {
  // evt.target is the button that was clicked
  var el = $(evt.target);
  // set parents attribute 'data-clicked' to the index of the clicked button

  var parent = el.parent();

  while(parent.attr('help-data') != 'removable'){
    parent = parent.parent();
  }

  var id = parent.attr('stored-data');
  parent.parent().attr('data-removed', id)
  parent.parent().trigger("change");
  parent.remove();
});

$(document).on("click", "button.progressPanel-close", function(evt) {
  // evt.target is the button that was clicked
  var el = $(evt.target);
  var parent = el.parent();

  while(parent.attr('help-data') != 'progressPanel'){
    parent = parent.parent();
  }

  parent.removeClass('slider-vis');
  parent.parent().removeClass('slider-top-vis');
});

$(document).on("click", "div.slider-top-vis", function(evt) {
  // evt.target is the button that was clicked
  var el = $(evt.target);
  var child = el.children();
  el.removeClass('slider-top-vis');
  child.removeClass('slider-vis');
});

var progressPanelBinding = new Shiny.InputBinding();
$.extend(progressPanelBinding, {
  find: function(scope) {
    return $(scope).find(".progress-panel");
  },
  getValue: function(el) {
    return $(el).attr('data-removed');
  },
  setValue: function(el, value) {
    $(el).attr('data-removed',value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.progressPanelBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".progressPanelBinding");
  }
});

Shiny.inputBindings.register(progressPanelBinding);

Shiny.addCustomMessageHandler('addProgressPanel', function(message) {
  var element = $("#" + message.element);
  var value  = message.html;

  element.html(element.html() + value);
});
Shiny.addCustomMessageHandler('removeProgressPanel', function(message) {
  var element = $("#" + message.element);
  var id  = message.id;

  element.children().each(function(){
    var dat = $(this).attr('stored-data');
    if(dat==id)$(this).remove();
  })
});
Shiny.addCustomMessageHandler('removeAllProgressPanel', function(message) {
  var element = $("#" + message.element);

  element.children().each(function(){
    $(this).remove();
  })
});

Shiny.addCustomMessageHandler('showProgressPanel', function(message) {
  var element = $("#" + message.element);
  element.parent().parent().parent().addClass('slider-vis');
  element.parent().parent().parent().parent().addClass('slider-top-vis');
});

Shiny.addCustomMessageHandler('hideProgressPanel', function(message) {
  var element = $("#" + message.element);
  element.parent().parent().parent().removeClass('slider-vis');
  element.parent().parent().parent().parent().removeClass('slider-top-vis');
});
