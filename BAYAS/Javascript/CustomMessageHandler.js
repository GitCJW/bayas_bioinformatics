$(document).ready(function() {
  Shiny.addCustomMessageHandler('resetValue', function(variableName) {
    Shiny.setInputValue(variableName, null);
    Shiny.onInputChange('select', 'null');
  });
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler('setValue', function(variableName, value) {
    Shiny.setInputValue(variableName, value);
  });
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler('highlightPickerInput', function(variableName) {
    var target = document.getElementById(variableName).nextSibling;
    target.style.border = 'solid 1px #1684c2';
    setTimeout(function(){target.style.border = ''}, 10000);
  });
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler('removeHighlightPickerInput', function(variableName) {
    var target = document.getElementById(variableName).nextSibling;
    target.style.border = '';
  });
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler('unbinding_table_elements', function(x) { 
    Shiny.unbindAll($(document.getElementById(x)).find('.dataTable'));
  });
});
