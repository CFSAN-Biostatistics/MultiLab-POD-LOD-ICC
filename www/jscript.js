
/* Enforce correct file extension on upload */
//https://stackoverflow.com/questions/62220495/r-shiny-restrict-fileinput-to-filename-pattern-and-not-just-file-type
//https://stackoverflow.com/questions/190852/how-can-i-get-file-extensions-with-javascript
//https://www.w3schools.com/howto/howto_css_modals.asp
function enforceFileExtension(file_object) {
  var file_name = file_object.value;
  //alert(file_name);  //for testing (filepath)
  var file_ext = file_name.split('.').pop();
  //alert(file_ext);
  var valid_exts = ['xlsx', 'XLSX'];
  if (! valid_exts.includes(file_ext)) {
      alert("Please select a file with extension .xlsx");
      file_object.value = "";
      //I may use a modal instead of an alert sometime in the future
      //var my_modal = document.getElementById("invalid_extension");  //ui.R
      //var my_span = document.getElementsByClassName("close")[0];
      //my_modal.style.display = "block";
      //my_span.onclick = function() {
      //  my_modal.style.display = "none"
      //}
      return false;
  }
  return true;
}


