
## Module overview site

# Hover effect of module planning
hoverScript  <- function(){

  paste0("
  $(function() {
    $('#module_planning_hover').hover(function() {
      $('#id_module_planning_description').css('color', '#555555');

      $('#image_module_evaluation').attr('src', 'Images/Modules/analysis_grey.png');
      $('#image_module_evaluation_click').css('border', '5px groove #555555');

      $('#image_module_report').attr('src', 'Images/Modules/report_grey.png');
      $('#image_module_report_click').css('border', '5px groove #555555');

      document.getElementById('id_module_transfer_1').style.display = 'block';
      document.getElementById('id_module_transfer_2').style.display = 'block';
      document.getElementById('id_module_transfer_3').style.display = 'block';


    }, function() {
      $('#id_module_planning_description').css('color', 'white');

      $('#image_module_evaluation').attr('src', 'Images/Modules/analysis.png');
      $('#image_module_evaluation_click').css('border', '5px groove #0295aa');

      $('#image_module_report').attr('src', 'Images/Modules/report.png');
      $('#image_module_report_click').css('border', '5px groove #445ea0');

      document.getElementById('id_module_transfer_1').style.display = 'none';
      document.getElementById('id_module_transfer_2').style.display = 'none';
      document.getElementById('id_module_transfer_3').style.display = 'none';


    });
  });

  $(function() {
    $('#module_evaluation_hover').hover(function() {
      $('#id_module_evaluation_description').css('color', '#555555');

      $('#image_module_planning').attr('src', 'Images/Modules/planning_3_grey.png');
      $('#image_module_planning_click').css('border', '5px groove #555555');
         
      $('#image_module_report').attr('src', 'Images/Modules/report_grey.png');
      $('#image_module_report_click').css('border', '5px groove #555555');

      document.getElementById('id_module_transfer_2').style.display = 'block';
      document.getElementById('id_module_transfer_3').style.display = 'block';

    }, function() {
      $('#id_module_evaluation_description').css('color', 'white');

      $('#image_module_planning').attr('src', 'Images/Modules/planning_3.png');
      $('#image_module_planning_click').css('border', '5px groove #fb8200');
         
      $('#image_module_report').attr('src', 'Images/Modules/report.png');
      $('#image_module_report_click').css('border', '5px groove #445ea0');

      document.getElementById('id_module_transfer_2').style.display = 'none';
      document.getElementById('id_module_transfer_3').style.display = 'none';

    });
  });

  $(function() {
    $('#module_report_hover').hover(function() {
      $('#id_module_report_description').css('color', '#555555');

      $('#image_module_planning').attr('src', 'Images/Modules/planning_3_grey.png');
      $('#image_module_planning_click').css('border', '5px groove #555555');
      
      $('#image_module_evaluation').attr('src', 'Images/Modules/analysis_grey.png');
      $('#image_module_evaluation_click').css('border', '5px groove #555555');

      document.getElementById('id_module_transfer_3').style.display = 'block';


    }, function() {
      $('#id_module_report_description').css('color', 'white');

      $('#image_module_planning').attr('src', 'Images/Modules/planning_3.png');
      $('#image_module_planning_click').css('border', '5px groove #fb8200');
         
      $('#image_module_evaluation').attr('src', 'Images/Modules/analysis.png');
      $('#image_module_evaluation_click').css('border', '5px groove #0295aa');

      document.getElementById('id_module_transfer_3').style.display = 'none';

    });
  });
  ")
}


