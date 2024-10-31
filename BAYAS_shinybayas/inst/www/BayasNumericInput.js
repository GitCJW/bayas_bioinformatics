$(document).on("input", "input.bayasNum", function(evt) {

  var el = $(evt.currentTarget);
  var rawVal = el.val();
  var val = asNumber(el.val());

  isInteger = el.attr('integer')==1;
  min = asNumber(el.attr('min'));
  max = asNumber(el.attr('max'));
  step = asNumber(el.attr('step'));
  emptyWarning = el.attr('emptyWarning');

  isNumeric = typeof val === 'number' && !isNaN(val);
  valid = true;

  if(isNumeric){

    if((val >= min) && (val <= max)){
      if(isInteger && Math.round(val)!=val){
        valid = false;
      }
    }else{
      valid = false;
    }
  }else{
    valid = false;
  }

  console.log(val);
  if(emptyWarning !== null && emptyWarning === "FALSE"){
    if(rawVal == null || rawVal == "") valid = true;
  }

  if(valid){
    el.attr('valid', 1);
    el.attr('class', "bayasNum form-control shinyjs-resettable shiny-bound-input");
    el.parent().parent().parent().children().last().children().attr('class', 'bayas-invis');
  }else{
    el.attr('valid', 0);
    el.attr('class', "bayasNum form-control bayas-invalid shinyjs-resettable shiny-bound-input");
    showMessages = el.attr('showMsg');
    if(showMessages=="TRUE") el.parent().parent().parent().children().last().children().attr('class', '');
  }

});

var isHolding = false;
var holdInterval;
var holdTimeout;
$(document).on("mousedown", "button.bayasNumInc", function(evt) {
  isHolding = true;

  var el = $(evt.currentTarget);
  var input = el.closest(".bayasNumEditWrapper").children().first();
  var isInteger = input.attr('integer')==1;
  var min = asNumber(input.attr('min'));
  var max = asNumber(input.attr('max'));
  var step = asNumber(input.attr('step'));

  var val = asNumber(input.val());
  if(!canInc(val, max)) return;
  val = getNextInc(val, max, min, isInteger, step);
  input.val(val);

  holdTimeout = setTimeout(() => {
    holdInterval = setInterval(function() {
      if (isHolding) {
        var val = asNumber(input.val());
        if(!canInc(val, max)) return;
        val = getNextInc(val, max, min, isInteger, step);
        input.val(val);
      }
    }, 50);
  }, 200);
});
$(document).on("mouseup mouseleave", "button.bayasNumInc", function(evt) {
  if(isHolding){
    var el = $(evt.currentTarget);
    var input = el.closest(".bayasNumEditWrapper").children().first();
    input.trigger("input");
  }
  clearTimeout(holdTimeout);
  clearInterval(holdInterval);
  isHolding = false;

});


$(document).on("mousedown", "button.bayasNumDec", function(evt) {
  isHolding = true;

  var el = $(evt.currentTarget);
  var input = el.closest(".bayasNumEditWrapper").children().first();
  var isInteger = input.attr('integer')==1;
  var min = asNumber(input.attr('min'));
  var max = asNumber(input.attr('max'));
  var step = asNumber(input.attr('step'));

  var val = asNumber(input.val());
  if(!canDec(val, min)) return;
  val = getNextDec(val, max, min, isInteger, step);
  input.val(val);

  holdTimeout = setTimeout(() => {
    holdInterval = setInterval(function() {
      if (isHolding) {
        var val = asNumber(input.val());
        if(!canDec(val, min)) return;
        val = getNextDec(val, max, min, isInteger, step);
        input.val(val);
      }
    }, 50);
  }, 200);
});
$(document).on("mouseup mouseleave", "button.bayasNumDec", function(evt) {
  if(isHolding){
    var el = $(evt.currentTarget);
    var input = el.closest(".bayasNumEditWrapper").children().first();
    input.trigger("input");
  }

  clearTimeout(holdTimeout);
  clearInterval(holdInterval);
  isHolding = false;

});

function asNumber(value){
  value = value.replace(/,/g, ".");
  return parseFloat(value);
}

function isNumber(value){
  return !isNaN(value); //typeof value === 'number' &&
}

function isValueInteger(value){
  if(isNumber(value) && Math.round(value)==value) return true;
  return false;
}

function countDecimalDigits(number) {
  // Convert number to string to handle cases like 1e-10 properly
  let numberStr = number.toString();

  // Check for scientific notation
  if (numberStr.includes('e')) {
      // Remove exponent part to count significant digits
      let parts = numberStr.split('e');
      let significand = parts[0].replace('.', '').replace('-', '');
      return significand.length - parseInt(parts[1], 10);
  }

  // Get decimal part of the number
  let decimalPart = (numberStr.split('.')[1] || '').replace(/0+$/, ''); // Remove trailing zeros

  return decimalPart.length;
}

function roundMeGood(a, b, valueNew){
  var digits = Math.max(countDecimalDigits(a), countDecimalDigits(b));
  return asNumber(valueNew.toFixed(digits));
}

function canInc(value, max) {
  if(isNumber(value) && value < max) return true;
  return false;
}
function getNextInc(value, max, min, isInteger, step) {
  if(canInc(value, max)){
    var newValue = value;
    if(isInteger && !isValueInteger(value)){
      newValue = Math.min(Math.ceil(value), max);
    }else{
      newValue = Math.min(Math.max(min, value+step), max);
    }
    value = roundMeGood(value, step, newValue);
  }
  return value;
}

function canDec(value, min) {
  if(isNumber(value) && value > min) return true;
  return false;
}
function getNextDec(value, max, min, isInteger, step) {
  if(canDec(value, min)){
    var newValue = value;
    if(isInteger && !isValueInteger(value)){
      newValue = Math.max(Math.floor(value), min)
    }else{
      newValue = Math.max(Math.min(max, value-step), min);
    }
    value = roundMeGood(value, step, newValue);
  }
  return value;
}





var bayasNumericInputBinding = new Shiny.InputBinding();
$.extend(bayasNumericInputBinding, {
  find: function(scope) {
    return $(scope).find(".bayasNum");
  },
  getValue: function(el) {
    var val = $(el).val();
    val = asNumber(val);

    isInteger = $(el).attr('integer')==1;
    min = asNumber($(el).attr('min'));
    max = asNumber($(el).attr('max'));

    isNumeric = isNumber(val);

    if(isNumeric){
      if((val >= min) && (val <= max)){
        if(isInteger && Math.round(val)!=val){
          return null;
        }
      }else{
        return null;
      }
      return val;
    }else{
      return null;
    }
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
    $(el).on("input.bayasNumericInputBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".bayasNumericInputBinding");
  }
});
Shiny.inputBindings.register(bayasNumericInputBinding);


Shiny.addCustomMessageHandler('updateBayasNumericInput', function(message) {

  var el = $("#" + message.element);
  var label = message.label;
  var val = message.value;
  var min = message.min;
  var max = message.max;
  var step = message.step;
  var integer = message.integer;
  var placeholder = message.placeholder;
  var emptyWarning = message.emptyWarning;
  var invalidMessage = message.invalidMessage;
  var invalidTooltip = message.invalidTooltip;
  var showMessages = message.showMessages;

  if(label !== null) el.parent().children().text(label);
  if(val !== null) el.val(val);
  if(placeholder !== null) el.placeholder(placeholder);
  if(min !== null) el.attr('min',min);
  if(max !== null) el.attr('max',max);
  if(step !== null) el.attr('step',step);
  if(showMessages !== null) el.attr('showMsg', showMessages);
  showMessages = el.attr('showMsg');

  if(integer !== null){
    if(integer){
      el.attr('integer', 1);
    }else{
      el.attr('integer', 0);
    }
  }
  if(invalidMessage !== null) el.parent().parent().parent().children().last().children().text(invalidMessage);
  if(invalidTooltip !== null) el.parent().parent().parent().children().last().children().attr('title', invalidTooltip);


  isInteger = el.attr('integer')==1;
  min = asNumber(el.attr('min'));
  max = asNumber(el.attr('max'));
  valid = true;

  if($.isNumeric(val)){
    if((val >= min) && (val <= max)){
      if(isInteger && Math.round(val)!=val){
        valid = false;
      }
    }else{
      valid = false;
    }
  }else{
    valid = false;
  }

  if(emptyWarning !== null && emptyWarning){
    if(val == null || val == "") valid = true;
  }

  if(valid){
    el.attr('valid', 1);
    el.attr('class', "bayasNum form-control shinyjs-resettable shiny-bound-input");
    el.parent().parent().parent().children().last().children().attr('class', 'bayas-invis');
  }else{
    el.attr('valid', 0);
    el.attr('class', "bayasNum form-control bayas-invalid shinyjs-resettable shiny-bound-input");
    if(showMessages=="TRUE") el.parent().parent().parent().children().last().children().attr('class', '');
  }

  el.trigger("input");
});
