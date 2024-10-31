
$(document).on("click", ".bayasBtnCollapse", function(evt) {

  // evt.target is the button that was clicked
  var el = $(evt.target);
  var parent = el;

  if(parent.prop("tagName").toLowerCase() == "i") parent = parent.parent();

  parent.children().first().toggleClass("fa-caret-left");
  parent.children().first().toggleClass("fa-caret-right");

  parent = parent.parent().parent();
  parent.toggleClass('bayas-sidePanel-open');
  parent.toggleClass('bayas-sidePanel-close');
});


Shiny.addCustomMessageHandler('updateBayasSidePanel', function(message) {

  var el = $("#" + message.element);
  var status = message.status;
  var icon = el.children().eq(1).children().first().children().first();

  if(status=="open"){
    icon.addClass("fa-caret-left");
    icon.removeClass("fa-caret-right");
    el.addClass('bayas-sidePanel-open');
    el.removeClass('bayas-sidePanel-close');
  }else{
    icon.addClass("fa-caret-right");
    icon.removeClass("fa-caret-left");
    el.removeClass('bayas-sidePanel-open');
    el.addClass('bayas-sidePanel-close');
  }

});
