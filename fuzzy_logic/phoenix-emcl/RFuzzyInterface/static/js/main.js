function showAddFunctionForm() {
    $('#add_FunctionForm').hide();
    $('#function_form_div').show();
}

function checkFunctionForm(type) {


    var domain_type = $('#id_domain_type').val();
    var domain_range = $('#id_domain_range').val();
    var expression = $('#id_expression').val();
    var attr_id = $("#id_attribute").val();
    var funcs = [];
    $("#function_list").children().each(function(index, value) {
                                            var val =value.getAttribute("value").split("\t");
                                            funcs.push( {"type":val[0], "range":val[1], "expression": val[2]});
                                        });
    funcs.push({"type":domain_type,"range":domain_range,"expression":expression});


    var url = "/"+type+"/check/";
    $.getJSON(url,
              {'attribute_id':attr_id,
               'funcs':funcs},
              function(data) {
                  if (data.status == true) {
                      var new_func = make_FunctionLink(domain_type, domain_range, expression);
                      $('#function_list').append(new_func);

                      $('#id_domain_type').val("");
                      $('#id_domain_range').val("");
                      $('#id_expression').val("");

                      $('#add_FunctionForm').show();
                      $('#function_form_div').hide();

                      $('#message_box').html("");
                      $('#message_box').hide();
                  } else if (data.status == false) {
                      $('#message_box').show();
                      $('#message_box').html("<p>"+data.message+"</p>");
                  }
              }
             );
}

function make_FunctionLink(type, range, expression){
    var val = type+"\t"+range+"\t"+expression;
    var id = Math.floor(Math.random()*1000);

    var div = $('<div></div>');
    div.attr("id", id);
    div.attr("value", val);

    if (type == "C")
        div.append("Continues ");
    else if (type == "D")
    div.append("Discrete ");

    div.append(range+" ");
    div.append(expression+" ");
    var remove_link = $('<a href="#">remove</a>');
    remove_link.click(function() {
                          $("#"+id).remove(); });
    div.append(remove_link);

    return div;
}

function submitEntity(func_type) {
    var funcs_str = "";
    $("#function_list").children().each(function(index, value) {
                                            var val =value.getAttribute("value");
                                            funcs_str += val;
                                            funcs_str += "\v";
                                        });
    $('#form_funcs').val(funcs_str);
    $('#main_form').submit();
}


function addSimpleQuery() {
    $('#form_query_list').show();
    var neg = $("#id_negation").val();
    var quant = $("#id_quantification").val();
    var concept = $("#id_concept").val();

    var link = make_QueryLink(neg, quant, concept);
    $("#query_list").append(link);

    $("#id_negation").val("");
    $("#id_quantification").val("");
    $("#id_concept").val("");
}

function make_QueryLink(neg, quant, concept) {
    var val = neg+"\t"+quant+"\t"+concept;
    var id = Math.floor(Math.random()*1000);

    var div = $('<div></div>');
    div.attr("id", id);
    div.attr("value", val);

    div.append(neg+" ");
    div.append(quant+" ");
    div.append(concept+" ");
    var remove_link = $('<a href="#">remove</a>');
    remove_link.click(function() {
                          $("#"+id).remove(); });
    div.append(remove_link);

    return div;
}

function submitQuery(){
    var query_str = "";
    $("#query_list").children().each(function(index, value) {
                                         var val =value.getAttribute("value");
                                         query_str += val;
                                         query_str += "\v";
                                     });
    $('#form_queries').val(query_str);
    $('#main_form').submit();
}

function deleteFunc(entity_type, id) {
    var url = "/"+entity_type+"/functions/delete/";
    $.get(url,
          {"id":id},
          function(data) {
              $("#"+id).remove();
          }
         );
}


function checkAndAddFunctionForm(type) {

    var domain_type = $('#id_domain_type').val();
    var domain_range = $('#id_domain_range').val();
    var expression = $('#id_expression').val();
    var attr_id = $("#id_attribute").val();
    var entity_name = $("#id_name").val();
    var funcs = [];
    $("#function_list").children().each(function(index, value) {
                                            var val =value.getAttribute("value").split("\t");
                                            funcs.push( {"type":val[0], "range":val[1], "expression": val[2]});
                                        });
    funcs.push({"type":domain_type,"range":domain_range,"expression":expression});


    var url = "/"+type+"/functions/checkandadd/";
    $.getJSON(url,
              {'attribute_id':attr_id,
               'entity_name':entity_name,
               'funcs':funcs},
              function(data) {
                  if (data.status == true) {

                      var div = $('<div></div>');
                      div.attr("id", data.id);
                      div.attr("value", domain_type+"\t"+domain_range+"\t"+expression);

                      if (domain_type == "C")
                          div.append("Continues ");
                      else if (domain_type == "D")
                      div.append("Discrete ");

                      div.append(domain_range+" ");
                      div.append(expression+" ");

                      var remove_link = $('<a href="#">remove</a>');
                      remove_link.click(function() {deleteFunc(type, data.id);});
                      div.append(remove_link);


                      $('#function_list').append(div);

                      $('#id_domain_type').val("");
                      $('#id_domain_range').val("");
                      $('#id_expression').val("");

                      $('#message_box').html("");
                      $('#message_box').hide();
                  } else if (data.status == false) {
                      $('#message_box').show();
                      $('#message_box').html("<p>"+data.message+"</p>");
                  }
              }
             );
}