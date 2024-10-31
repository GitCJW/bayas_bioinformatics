$(document).ready(function() {
  var isDragging = false;

  $('.dragHandleUploadPanels').mousedown(function(e) {
    console.log("click_down");
    isDragging = true;
    e.preventDefault();
  });

  $(document).mouseup(function() {
    isDragging = false;
  });

  $(document).mousemove(function(e) {
    if (isDragging) {

      var width = $('#dragHandleUploadPanelsDiv').width();
      var parentWidth = $('#userUploadPanelLeft').parent().width();
      var cur = e.pageX - width/2;
      
      var leftBasis = 0;
      var rightBasis = 0;
      if(cur < parentWidth/2){
        if(cur > 0){
          rightBasis = parentWidth - Math.max(parentWidth/1.5, 2*cur);
          rightBasis = rightBasis/parentWidth *100;
        }
      }else{
        if(cur < parentWidth){
          leftBasis = Math.min(parentWidth/3, cur*2-parentWidth);
          leftBasis = leftBasis/parentWidth *100;
        }
      }
      
      $('#userUploadPanelLeft').css('flex', '1 0 ' + leftBasis  + '%');
      $('#userUploadPanelRight').css('flex', '1 0 ' + rightBasis + '%');
    }
  });
});

document.addEventListener('DOMContentLoaded', function() {
  var divElement = document.getElementsByClassName('dragHandleUploadPanels');
  
  for (var i = 0; i < divElement.length; i++) {
    divElement[i].addEventListener('dblclick', function(event) {
      var left = document.getElementById("userUploadPanelLeft");
      var right = document.getElementById("userUploadPanelRight");
      left.style.flex = "1";
      right.style.flex = "1";
    });
  }

});