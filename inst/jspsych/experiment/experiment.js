var safe_to_close_window = false;
function verifyClose() {
  if (safe_to_close_window == false) {
    event.returnValue = "Are you sure you want to exit? Your data has not been saved yet.";
  }
}
window.onbeforeunload = verifyClose;

var awareness_condition, condition, distribution_condition, presentation_condition, regex_awareness, regex_distribution, regex_presentation, urlvar;

condition = jsPsych.randomization.sampleWithoutReplacement(["naive_separate_absent", "naive_joint_absent", "naive_separate_present", "aware_separate_absent"], 1)[0];

regex_awareness = /(.*)_.*_.*/;

regex_presentation = /.*_(.*)_.*/;

regex_distribution = /.*_.*_(.*)/;

awareness_condition = condition.match(regex_awareness)[1];

presentation_condition = condition.match(regex_presentation)[1];

distribution_condition = condition.match(regex_distribution)[1];

urlvar = jsPsych.data.urlVariables();

if (typeof urlvar.presentation !== 'undefined') {
  presentation_condition = urlvar.presentation;
}

if (typeof urlvar.awareness !== 'undefined') {
  awareness_condition = urlvar.awareness;
}

if (typeof urlvar.distribution !== 'undefined') {
  distribution_condition = urlvar.distribution;
}

function checkOther(val, id){
 var element=document.getElementById(id);
 if(val=='Other')
   element.style.display='block';
 else
   element.style.display='none';
}
jsPsych.data.addProperties({
  "subject": jsPsych.randomization.randomID(15),
  "experiment": "aggregation_exp2",
  "sample": "prolific",
  "awareness": awareness_condition,
  "presentation": presentation_condition,
  "distribution": distribution_condition
});

var timeline = {
  "timeline": [
    {
      "type": ["instructions"],
      "pages": ["<div>\n  <p>Welcome to the study.<\/p>\n  <p>Make sure to scroll down to the bottom of each page to see the navigation buttons.<\/p>\n<\/div>"],
      "key_forward": [39],
      "key_backward": [37],
      "allow_backward": true,
      "allow_keys": true,
      "show_clickable_nav": true,
      "button_label_previous": ["Previous"],
      "button_label_next": ["Next"],
      "post_trial_gap": [0],
      "data": {
        "stage": ["welcome"]
      }
    },
    {
      "timeline": [
        {
          "type": ["survey-html-form"],
          "html": ["<div>\n  <label for=\"prolific\">Enter your Prolific ID (24 alphanumeric characters, no spaces):<\/label>\n  <input type=\"text\" id=\"prolific\" name=\"prolific\" required minlength=\"24\" maxlength=\"24\" pattern=\"^[a-z0-9]+$\" size=\"30\"/>\n<\/div>"],
          "data": {
            "stage": ["id"]
          },
          "on_load": function() {
      if (typeof urlvar.PROLIFIC_PID !== 'undefined') {
        document.getElementById('prolific').setAttribute('value', urlvar.PROLIFIC_PID);
      }
    }
        },
        {
          "type": ["survey-html-form"],
          "html": ["<div>\n  <div>\n    <p>I would like to receive feedback about the overall results of this study.<\/p>\n    <input type=\"radio\" id=\"contact_yes\" name=\"contact\" value=\"yes\"/>\n    <label for=\"contact_yes\">YES<\/label>\n    <input type=\"radio\" id=\"contact_no\" name=\"contact\" value=\"no\" checked/>\n    <label for=\"contact_no\">NO<\/label>\n  <\/div>\n  <div>\n    <p>If you answered YES, please indicate your preferred form of feedback and address:<\/p>\n    <p>\n      Email:\n      <input type=\"text\" id=\"address\" name=\"address\"/>\n    <\/p>\n  <\/div>\n<\/div>"],
          "data": {
            "stage": ["contact"]
          }
        },
        {
          "type": ["survey-html-form"],
          "html": ["<ol style=\"text-align:left\">\n  <li>\n    <p>\n      <p>What is your sex?<\/p>\n      <input type=\"radio\" id=\"male\" name=\"sex\" value=\"male\" checked/>\n      <label for=\"male\">Male<\/label>\n      <input type=\"radio\" id=\"female\" name=\"sex\" value=\"female\"/>\n      <label for=\"female\">Female<\/label>\n    <\/p>\n  <\/li>\n  <li>\n    <p>\n      <p>\n        <label for=\"age\">What is your age?<\/label>\n      <\/p>\n      \n      <input type=\"number\" id=\"age\" name=\"age\" min=\"10\" max=\"100\" required style=\"width:70px\"/>\n      \n    <\/p>\n  <\/li>\n  <li><p>\n  <p>\n    <p>\n      <label for=\"language\">Do you speak a language other than English at home?<\/label>\n    <\/p>\n    <select id=\"language\" name=\"language\" onchange=\"checkOther(this.value, 'language_other');\" required>\n      <option value=\"\"><\/option>\n      <option value=\"No\">No<\/option>\n      <option value=\"Chinese\">Chinese<\/option>\n      <option value=\"Japanese\">Japanese<\/option>\n      <option value=\"Vietnamese\">Vietnamese<\/option>\n      <option value=\"Korean\">Korean<\/option>\n      <option value=\"Arabic\">Arabic<\/option>\n      <option value=\"Spanish\">Spanish<\/option>\n      <option value=\"Italian\">Italian<\/option>\n      <option value=\"Greek\">Greek<\/option>\n      <option value=\"Hebrew\">Hebrew<\/option>\n      <option value=\"Other\">Other<\/option>\n    <\/select>\n  <\/p>\n  <p>\n    <input type=\"text\" id=\"language_other\" name=\"language_other\" style=\"display:none;\" placeholder=\"Specify other\"/>\n  <\/p>\n<\/p><\/li>\n  <li>\n    <p>\n      <p>\n        <label for=\"business_edu\">How many years of experience do you have studying business?<\/label>\n      <\/p>\n      \n      <input type=\"number\" id=\"business_edu\" name=\"business_edu\" min=\"0\" max=\"100\" required style=\"width:70px\"/>\n      years\n    <\/p>\n  <\/li>\n  <li>\n    <p>\n      <p>\n        <label for=\"business_exp\">How many years of experience do you have working in a corporate business setting?<\/label>\n      <\/p>\n      \n      <input type=\"number\" id=\"business_exp\" name=\"business_exp\" min=\"0\" max=\"100\" required style=\"width:70px\"/>\n      years\n    <\/p>\n  <\/li>\n  <li>\n    <p>\n      <p>Do you currently work in an executive or managerial role?<\/p>\n      <input type=\"radio\" id=\"current_yes\" name=\"current\" value=\"yes\"/>\n      <label for=\"current_yes\">Yes<\/label>\n      <input type=\"radio\" id=\"current_no\" name=\"current\" value=\"no\" checked/>\n      <label for=\"current_no\">No<\/label>\n    <\/p>\n  <\/li>\n<\/ol>"],
          "data": {
            "stage": ["demographics"]
          },
          "on_finish": function(data){
    data.current_response = JSON.parse(data.responses).current
  }
        },
        {
          "timeline": [
            {
              "type": ["survey-html-form"],
              "html": ["<ol style=\"text-align:left\">\n  <li>\n    <p>\n      <label for=\"company_name\">What is your company name? (optional)<\/label>\n      <input type=\"text\" id=\"company_name\" name=\"company_name\"/>\n    <\/p>\n  <\/li>\n  <li><p>\n  <p>\n    <p>\n      <label for=\"sector\">What is your primary company sector?<\/label>\n    <\/p>\n    <select id=\"sector\" name=\"sector\" onchange=\"checkOther(this.value, 'sector_other');\" required>\n      <option value=\"\"><\/option>\n      <option value=\"Aerospace &amp; Defense\">Aerospace &amp; Defense<\/option>\n      <option value=\"Agriculture, Farming, Food Processing\">Agriculture, Farming, Food Processing<\/option>\n      <option value=\"Asset Management - Brokers, Fund Mgmt, Stock Exchange\">Asset Management - Brokers, Fund Mgmt, Stock Exchange<\/option>\n      <option value=\"Automotive\">Automotive<\/option>\n      <option value=\"Banking - Payments, Central Banks, Retail, Wholesale, Investment\">Banking - Payments, Central Banks, Retail, Wholesale, Investment<\/option>\n      <option value=\"Business Services - Legal, Accounting, Security, Consulting, PR\">Business Services - Legal, Accounting, Security, Consulting, PR<\/option>\n      <option value=\"Chemicals\">Chemicals<\/option>\n      <option value=\"Computers &amp; Electronics, Software, High Tech\">Computers &amp; Electronics, Software, High Tech<\/option>\n      <option value=\"Consumer Products &amp; Packaged Goods\">Consumer Products &amp; Packaged Goods<\/option>\n      <option value=\"Diversified Conglomerate\">Diversified Conglomerate<\/option>\n      <option value=\"Energy - Power, Gas, Electricity, Water, Renewables\">Energy - Power, Gas, Electricity, Water, Renewables<\/option>\n      <option value=\"Engineering, Construction, Infrastructure, Real Estate\">Engineering, Construction, Infrastructure, Real Estate<\/option>\n      <option value=\"Health Services - Payor, Provider\">Health Services - Payor, Provider<\/option>\n      <option value=\"Insurance - Pension, Financial, Health \tLeisure - Sports, Parks, Theater, Gambling\">Insurance - Pension, Financial, Health \tLeisure - Sports, Parks, Theater, Gambling<\/option>\n      <option value=\"Machinery &amp; Industrial Goods\">Machinery &amp; Industrial Goods<\/option>\n      <option value=\"Media - Broadcasting, Publishing, Information\">Media - Broadcasting, Publishing, Information<\/option>\n      <option value=\"Metals, Mining, Ore\">Metals, Mining, Ore<\/option>\n      <option value=\"Paper, Packaging, Forestry\">Paper, Packaging, Forestry<\/option>\n      <option value=\"Petroleum, Oil\">Petroleum, Oil<\/option>\n      <option value=\"Pharmaceuticals, Medical Products &amp; Equipment\">Pharmaceuticals, Medical Products &amp; Equipment<\/option>\n      <option value=\"Private Equity, FOB, Venture Capital\">Private Equity, FOB, Venture Capital<\/option>\n      <option value=\"Public Sector, Government\">Public Sector, Government<\/option>\n      <option value=\"Retail, Wholesale, Restaurants\">Retail, Wholesale, Restaurants<\/option>\n      <option value=\"Social Sector, Non-profit, Charity\">Social Sector, Non-profit, Charity<\/option>\n      <option value=\"Telecommunications\">Telecommunications<\/option>\n      <option value=\"Travel, Transport &amp; Logistics, Hotels\">Travel, Transport &amp; Logistics, Hotels<\/option>\n      <option value=\"Other\">Other<\/option>\n    <\/select>\n  <\/p>\n  <p>\n    <input type=\"text\" id=\"sector_other\" name=\"sector_other\" style=\"display:none;\" placeholder=\"Specify other\"/>\n  <\/p>\n<\/p><\/li>\n  <li>\n    <p>\n      <p>\n        <label for=\"employees\">Approximately, how many employees work at your company?<\/label>\n      <\/p>\n      \n      <input type=\"number\" id=\"employees\" name=\"employees\" min=\"0\" max=\"3e+06\" required style=\"width:70px\"/>\n      \n    <\/p>\n  <\/li>\n  <li>\n    <p>\n      <p>\n        <label for=\"revenue\">What is the size of your company's revenues?<\/label>\n      <\/p>\n      $\n      <input type=\"number\" id=\"revenue\" name=\"revenue\" min=\"0\" max=\"6e+05\" required style=\"width:70px\"/>\n      million\n    <\/p>\n  <\/li>\n  <li><p>\n  <p>\n    <p>\n      <label for=\"role_company\">What is your role in the company?<\/label>\n    <\/p>\n    <select id=\"role_company\" name=\"role_company\" onchange=\"checkOther(this.value, 'role_company_other');\" required>\n      <option value=\"\"><\/option>\n      <option value=\"CEO\">CEO<\/option>\n      <option value=\"Corporate President\">Corporate President<\/option>\n      <option value=\"Chairman\">Chairman<\/option>\n      <option value=\"Managing Director\">Managing Director<\/option>\n      <option value=\"CFO\">CFO<\/option>\n      <option value=\"COO\">COO<\/option>\n      <option value=\"CSO\">CSO<\/option>\n      <option value=\"Other C-level executive\">Other C-level executive<\/option>\n      <option value=\"Senior strategist\">Senior strategist<\/option>\n      <option value=\"EVP\">EVP<\/option>\n      <option value=\"BU/Divisional President\">BU/Divisional President<\/option>\n      <option value=\"SVP\">SVP<\/option>\n      <option value=\"VP\">VP<\/option>\n      <option value=\"Business Leader\">Business Leader<\/option>\n      <option value=\"Business Member\">Business Member<\/option>\n      <option value=\"Other\">Other<\/option>\n    <\/select>\n  <\/p>\n  <p>\n    <input type=\"text\" id=\"role_company_other\" name=\"role_company_other\" style=\"display:none;\" placeholder=\"Specify other\"/>\n  <\/p>\n<\/p><\/li>\n  <li><p>\n  <p>\n    <p>\n      <label for=\"role_allocation\">What is your role in resource allocation decisions?<\/label>\n    <\/p>\n    <select id=\"role_allocation\" name=\"role_allocation\" onchange=\"checkOther(this.value, 'role_allocation_other');\" required>\n      <option value=\"\"><\/option>\n      <option value=\"Decide on allocations for the company\">Decide on allocations for the company<\/option>\n      <option value=\"Decide on allocations for business unit or division\">Decide on allocations for business unit or division<\/option>\n      <option value=\"Decide on allocations for line of business level\">Decide on allocations for line of business level<\/option>\n      <option value=\"Provide information and analysis for the decision-makers at the company\">Provide information and analysis for the decision-makers at the company<\/option>\n      <option value=\"Provide information and analysis for the decision-makers at business unit or division\">Provide information and analysis for the decision-makers at business unit or division<\/option>\n      <option value=\"Provide information and analysis for the decision-makers at line of business level\">Provide information and analysis for the decision-makers at line of business level<\/option>\n      <option value=\"Other\">Other<\/option>\n    <\/select>\n  <\/p>\n  <p>\n    <input type=\"text\" id=\"role_allocation_other\" name=\"role_allocation_other\" style=\"display:none;\" placeholder=\"Specify other\"/>\n  <\/p>\n<\/p><\/li>\n  <li>\n    <p>\n      <p>\n        <label for=\"budget\">How large is the annual budget under your discretion?<\/label>\n      <\/p>\n      $\n      <input type=\"number\" id=\"budget\" name=\"budget\" min=\"0\" max=\"6e+05\" required style=\"width:70px\"/>\n      million\n    <\/p>\n  <\/li>\n<\/ol>"],
              "data": {
                "stage": ["business_information"]
              }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.current_response == "yes"){
        return true;
      } else {
        return false;
      }
    }
        }
      ]
    },
    {
      "timeline": [
        {
          "type": ["instructions"],
          "pages": ["We will now give you the instructions for the task. Use the arrow buttons to browse these instructions", "<div>\n  <p>Imagine that you are an executive in a large company composed of many individual businesses. You will see various projects from these businesses and have to decide whether you would like to invest in them.<\/p>\n  <p>Imagine that making good investment decisions will result in you receiving a generous bonus and a potential promotion, and that doing poorly will result in you receiving a large pay cut and a potential demotion. We want to know what choices you would actually make in these scenarios.<\/p>\n<\/div>", "Press the 'Next' button to begin."],
          "key_forward": [39],
          "key_backward": [37],
          "allow_backward": true,
          "allow_keys": true,
          "show_clickable_nav": true,
          "button_label_previous": ["Previous"],
          "button_label_next": ["Next"],
          "post_trial_gap": [0],
          "data": {
            "stage": ["instructions"]
          }
        },
        {
          "timeline": [
            {
              "type": ["html-button-response"],
              "stimulus": ["You will now see the 10 projects."],
              "choices": ["Continue"],
              "margin_vertical": ["0px"],
              "margin_horizontal": ["8px"],
              "response_ends_trial": true,
              "post_trial_gap": [0],
              "data": {
                "stage": ["awareness"]
              }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.awareness == "aware"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "type": ["html-button-response"],
              "stimulus": ["You will now see the projects."],
              "choices": ["Continue"],
              "margin_vertical": ["0px"],
              "margin_horizontal": ["8px"],
              "response_ends_trial": true,
              "post_trial_gap": [0],
              "data": {
                "stage": ["awareness"]
              }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.awareness == "naive"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "timeline": [
                {
                  "type": ["survey-multi-choice"],
                  "questions": [
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $40 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 30% chance of the project succeeding. Therefore, <strong>there is 30% chance of gaining $200 million and a 70% chance of losing $40 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_200_240_0.3"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $110 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $130 million and a 35% chance of losing $110 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_130_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $100 million and a 30% chance of losing $140 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_100_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $100 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 55% chance of the project succeeding. Therefore, <strong>there is 55% chance of gaining $140 million and a 45% chance of losing $100 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_140_240_0.55"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $50 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $190 million and a 65% chance of losing $50 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_190_240_0.35"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 75% chance of the project succeeding. Therefore, <strong>there is 75% chance of gaining $100 million and a 25% chance of losing $140 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_100_240_0.75"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $110 million and a 30% chance of losing $130 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_110_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $110 million and a 35% chance of losing $130 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_110_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 40% chance of the project succeeding. Therefore, <strong>there is 40% chance of gaining $180 million and a 60% chance of losing $60 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_180_240_0.4"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $180 million and a 65% chance of losing $60 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_180_240_0.35"]
                    }
                  ],
                  "randomize_question_order": true,
                  "preamble": ["<div>\n  <p>Below is the probability distribution of final outcomes if all projects were chosen.<\/p>\n  <p>The numbers on the x-axis (labelled 'Outcome') represent the final amounts of money possible if you chose to invest in all the projects. The numbers on the y-axis (labelled 'Probability') represent the likelihoods of each of the possible outcomes. Negative final outcomes (losses) are shown in red, positive final outcomes (gains) are shown in green, and a final outcome of zero (no loss or gain) is shown in blue.<\/p>\n<\/div><div><img src=\"resource/image/gambles_plot_experiment2.png\" width=\"600\" height=\"400\"/><\/div><p>Indicate below whether you would invest in the following:<\/p>"],
                  "button_label": ["Continue"],
                  "required_message": ["You must choose at least one response for this question"],
                  "post_trial_gap": [0],
                  "data": {
                    "stage": ["project_choice"]
                  }
                }
              ],
              "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "present"){
        return true;
      } else {
        return false;
      }
    }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.presentation == "joint"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "timeline": [
                {
                  "type": ["survey-multi-choice"],
                  "questions": [
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $40 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 30% chance of the project succeeding. Therefore, <strong>there is 30% chance of gaining $200 million and a 70% chance of losing $40 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_200_240_0.3"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $110 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $130 million and a 35% chance of losing $110 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_130_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $100 million and a 30% chance of losing $140 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_100_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $100 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 55% chance of the project succeeding. Therefore, <strong>there is 55% chance of gaining $140 million and a 45% chance of losing $100 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_140_240_0.55"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $50 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $190 million and a 65% chance of losing $50 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_190_240_0.35"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 75% chance of the project succeeding. Therefore, <strong>there is 75% chance of gaining $100 million and a 25% chance of losing $140 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_100_240_0.75"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $110 million and a 30% chance of losing $130 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_110_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $110 million and a 35% chance of losing $130 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_110_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 40% chance of the project succeeding. Therefore, <strong>there is 40% chance of gaining $180 million and a 60% chance of losing $60 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_180_240_0.4"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $180 million and a 65% chance of losing $60 million on the investment.<\/strong>"],
                      "options": ["Yes", "No"],
                      "horizontal": false,
                      "required": true,
                      "name": ["oil-well_180_240_0.35"]
                    }
                  ],
                  "randomize_question_order": true,
                  "preamble": ["<p>Indicate below whether you would invest in the following:<\/p>"],
                  "button_label": ["Continue"],
                  "required_message": ["You must choose at least one response for this question"],
                  "post_trial_gap": [0],
                  "data": {
                    "stage": ["project_choice"]
                  }
                }
              ],
              "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "absent"){
        return true;
      } else {
        return false;
      }
    }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.presentation == "joint"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "timeline": [
                {
                  "timeline": [
                    {
                      "type": ["survey-multi-choice"],
                      "questions": [
                        {
                          "prompt": jsPsych.timelineVariable('prompt'),
                          "options": ["Yes", "No"],
                          "horizontal": false,
                          "required": true,
                          "name": jsPsych.timelineVariable('name')
                        }
                      ],
                      "randomize_question_order": false,
                      "preamble": ["<div>\n  <p>Below is the probability distribution of final outcomes if all projects were chosen.<\/p>\n  <p>The numbers on the x-axis (labelled 'Outcome') represent the final amounts of money possible if you chose to invest in all the projects. The numbers on the y-axis (labelled 'Probability') represent the likelihoods of each of the possible outcomes. Negative final outcomes (losses) are shown in red, positive final outcomes (gains) are shown in green, and a final outcome of zero (no loss or gain) is shown in blue.<\/p>\n<\/div><div><img src=\"resource/image/gambles_plot_experiment2.png\" width=\"600\" height=\"400\"/><\/div><p>Indicate below whether you would invest in the following:<\/p>"],
                      "button_label": ["Continue"],
                      "required_message": ["You must choose at least one response for this question"],
                      "post_trial_gap": [0],
                      "data": {
                        "stage": ["project_choice"]
                      }
                    }
                  ],
                  "timeline_variables": [
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $40 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 30% chance of the project succeeding. Therefore, <strong>there is 30% chance of gaining $200 million and a 70% chance of losing $40 million on the investment.<\/strong>"],
                      "name": ["oil-well_200_240_0.3"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $110 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $130 million and a 35% chance of losing $110 million on the investment.<\/strong>"],
                      "name": ["oil-well_130_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $100 million and a 30% chance of losing $140 million on the investment.<\/strong>"],
                      "name": ["oil-well_100_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $100 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 55% chance of the project succeeding. Therefore, <strong>there is 55% chance of gaining $140 million and a 45% chance of losing $100 million on the investment.<\/strong>"],
                      "name": ["oil-well_140_240_0.55"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $50 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $190 million and a 65% chance of losing $50 million on the investment.<\/strong>"],
                      "name": ["oil-well_190_240_0.35"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 75% chance of the project succeeding. Therefore, <strong>there is 75% chance of gaining $100 million and a 25% chance of losing $140 million on the investment.<\/strong>"],
                      "name": ["oil-well_100_240_0.75"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $110 million and a 30% chance of losing $130 million on the investment.<\/strong>"],
                      "name": ["oil-well_110_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $110 million and a 35% chance of losing $130 million on the investment.<\/strong>"],
                      "name": ["oil-well_110_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 40% chance of the project succeeding. Therefore, <strong>there is 40% chance of gaining $180 million and a 60% chance of losing $60 million on the investment.<\/strong>"],
                      "name": ["oil-well_180_240_0.4"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $180 million and a 65% chance of losing $60 million on the investment.<\/strong>"],
                      "name": ["oil-well_180_240_0.35"]
                    }
                  ],
                  "randomize_order": true
                }
              ],
              "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "present"){
        return true;
      } else {
        return false;
      }
    }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.presentation == "separate"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "timeline": [
                {
                  "timeline": [
                    {
                      "type": ["survey-multi-choice"],
                      "questions": [
                        {
                          "prompt": jsPsych.timelineVariable('prompt'),
                          "options": ["Yes", "No"],
                          "horizontal": false,
                          "required": true,
                          "name": jsPsych.timelineVariable('name')
                        }
                      ],
                      "randomize_question_order": false,
                      "preamble": ["<p>Indicate below whether you would invest in the following:<\/p>"],
                      "button_label": ["Continue"],
                      "required_message": ["You must choose at least one response for this question"],
                      "post_trial_gap": [0],
                      "data": {
                        "stage": ["project_choice"]
                      }
                    }
                  ],
                  "timeline_variables": [
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $40 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 30% chance of the project succeeding. Therefore, <strong>there is 30% chance of gaining $200 million and a 70% chance of losing $40 million on the investment.<\/strong>"],
                      "name": ["oil-well_200_240_0.3"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $110 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $130 million and a 35% chance of losing $110 million on the investment.<\/strong>"],
                      "name": ["oil-well_130_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $100 million and a 30% chance of losing $140 million on the investment.<\/strong>"],
                      "name": ["oil-well_100_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $100 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 55% chance of the project succeeding. Therefore, <strong>there is 55% chance of gaining $140 million and a 45% chance of losing $100 million on the investment.<\/strong>"],
                      "name": ["oil-well_140_240_0.55"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $50 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $190 million and a 65% chance of losing $50 million on the investment.<\/strong>"],
                      "name": ["oil-well_190_240_0.35"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $140 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 75% chance of the project succeeding. Therefore, <strong>there is 75% chance of gaining $100 million and a 25% chance of losing $140 million on the investment.<\/strong>"],
                      "name": ["oil-well_100_240_0.75"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 70% chance of the project succeeding. Therefore, <strong>there is 70% chance of gaining $110 million and a 30% chance of losing $130 million on the investment.<\/strong>"],
                      "name": ["oil-well_110_240_0.7"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $130 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 65% chance of the project succeeding. Therefore, <strong>there is 65% chance of gaining $110 million and a 35% chance of losing $130 million on the investment.<\/strong>"],
                      "name": ["oil-well_110_240_0.65"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 40% chance of the project succeeding. Therefore, <strong>there is 40% chance of gaining $180 million and a 60% chance of losing $60 million on the investment.<\/strong>"],
                      "name": ["oil-well_180_240_0.4"]
                    },
                    {
                      "prompt": ["Refinera is a business in your company that proposes to construct an oil well project, which they forecast will cost $60 million. If the project succeeds, forecasts show the company would make $240 million. Research suggests that there is a 35% chance of the project succeeding. Therefore, <strong>there is 35% chance of gaining $180 million and a 65% chance of losing $60 million on the investment.<\/strong>"],
                      "name": ["oil-well_180_240_0.35"]
                    }
                  ],
                  "randomize_order": true
                }
              ],
              "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "absent"){
        return true;
      } else {
        return false;
      }
    }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.presentation == "separate"){
        return true;
      } else {
        return false;
      }
    }
        }
      ]
    },
    {
      "timeline": [
        {
          "type": ["survey-html-form"],
          "html": ["<p>\n  <p>\n    <label for=\"project_number\">In total, how many projects did you just see?<\/label>\n  <\/p>\n  \n  <input type=\"number\" id=\"project_number\" name=\"project_number\" min=\"0\" max=\"20\" required style=\"width:70px\"/>\n  projects\n<\/p>"],
          "data": {
            "stage": ["project_number"]
          }
        },
        {
          "timeline": [
            {
              "type": ["html-button-response"],
              "stimulus": ["<div>\n  <p>Below is the probability distribution of final outcomes if all projects were chosen.<\/p>\n  <p>The numbers on the x-axis (labelled 'Outcome') represent the final amounts of money possible if you chose to invest in all the projects. The numbers on the y-axis (labelled 'Probability') represent the likelihoods of each of the possible outcomes. Negative final outcomes (losses) are shown in red, positive final outcomes (gains) are shown in green, and a final outcome of zero (no loss or gain) is shown in blue.<\/p>\n<\/div><div><img src=\"resource/image/gambles_plot_experiment2.png\" width=\"600\" height=\"400\"/><\/div><p>Indicate below whether you would invest in the following:<\/p><p>\n  <strong>Consider all the projects you saw. If you had to choose between investing in all of them, or investing in none of them, which would you choose?<\/strong>\n<\/p>"],
              "choices": ["Invest in all of the projects", "Invest in none of the projects"],
              "margin_vertical": ["0px"],
              "margin_horizontal": ["8px"],
              "response_ends_trial": true,
              "post_trial_gap": [0],
              "data": {
                "stage": ["portfolio_binary"]
              }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "present"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "type": ["html-button-response"],
              "stimulus": ["<p>\n  <strong>Consider all the projects you saw. If you had to choose between investing in all of them, or investing in none of them, which would you choose?<\/strong>\n<\/p>"],
              "choices": ["Invest in all of the projects", "Invest in none of the projects"],
              "margin_vertical": ["0px"],
              "margin_horizontal": ["8px"],
              "response_ends_trial": true,
              "post_trial_gap": [0],
              "data": {
                "stage": ["portfolio_binary"]
              }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "absent"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "type": ["survey-html-form"],
              "html": ["<div>\n  <p>Below is the probability distribution of final outcomes if all projects were chosen.<\/p>\n  <p>The numbers on the x-axis (labelled 'Outcome') represent the final amounts of money possible if you chose to invest in all the projects. The numbers on the y-axis (labelled 'Probability') represent the likelihoods of each of the possible outcomes. Negative final outcomes (losses) are shown in red, positive final outcomes (gains) are shown in green, and a final outcome of zero (no loss or gain) is shown in blue.<\/p>\n<\/div><div><img src=\"resource/image/gambles_plot_experiment2.png\" width=\"600\" height=\"400\"/><\/div><p>Indicate below whether you would invest in the following:<\/p><p>\n  <p>\n    <label for=\"portfolio_number\">\n      <p>\n        <strong>\n          The total number of projects you were shown is \n          10\n          . If you could choose to invest in a certain number of those \n          10\n           projects, how many would you invest in?\n        <\/strong>\n      <\/p>\n    <\/label>\n  <\/p>\n  \n  <input type=\"number\" id=\"portfolio_number\" name=\"portfolio_number\" min=\"0\" max=\"10\" required style=\"width:70px\"/>\n  projects\n<\/p>"],
              "data": {
                "stage": ["portfolio_number"]
              }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "present"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "timeline": [
            {
              "type": ["survey-html-form"],
              "html": ["<p>\n  <p>\n    <label for=\"portfolio_number\">\n      <p>\n        <strong>\n          The total number of projects you were shown is \n          10\n          . If you could choose to invest in a certain number of those \n          10\n           projects, how many would you invest in?\n        <\/strong>\n      <\/p>\n    <\/label>\n  <\/p>\n  \n  <input type=\"number\" id=\"portfolio_number\" name=\"portfolio_number\" min=\"0\" max=\"10\" required style=\"width:70px\"/>\n  projects\n<\/p>"],
              "data": {
                "stage": ["portfolio_number"]
              }
            }
          ],
          "conditional_function": function(){
      var data = jsPsych.data.get().last(1).values()[0];
      if(data.distribution == "absent"){
        return true;
      } else {
        return false;
      }
    }
        },
        {
          "type": ["html-button-response"],
          "stimulus": ["<div>\n  <p>Press below to complete the experiment.<\/p>\n  <p>The next page will be a blank white screen. It will take approximately 10 seconds to save your data, after which you will be automatically redirected back to Prolific. Please do not exit until you have been redirected back to Prolific.<\/p>\n  <p>Thank you!<\/p>\n<\/div>"],
          "choices": ["End experiment"],
          "margin_vertical": ["0px"],
          "margin_horizontal": ["8px"],
          "response_ends_trial": true,
          "post_trial_gap": [0],
          "data": {
            "stage": ["end"]
          }
        }
      ]
    }
  ]
};

jsPsych.init(
{
  "timeline": [timeline],
  "experiment_width": [750],
  "preload_images": [],
  "on_finish": {}
}
);
