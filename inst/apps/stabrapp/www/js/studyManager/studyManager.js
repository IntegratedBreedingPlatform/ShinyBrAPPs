/*global getJquerySafeId, showErrorMessage, oldLineSelected, changeAdvanceBreedingMethod, setCorrecMethodValues, oldMethodSelected, msgSamplePlotError, msgHarvestDateError, methodSuggestionsFav_obj, isInt, breedingMethodId, oldMethodSelected*/
/*global isStudyNameUnique, validateStartDateEndDateBasic*/

function checkMethod() { //TODO ADVANCE
	'use strict';
	var methodChoiceCheckbox = $('input[type=checkbox][name=methodChoice]');

	if (!methodChoiceCheckbox.length) {
		return;
	}

	if (methodChoiceCheckbox.is(':checked')) {
		$('#methodIdFavorite').select2('enable', true);
		$('#methodIdAll').select2('enable', true);
		$('#methodIdDerivativeAndMaintenance').select2('enable', true);
		$('#methodIdDerivativeAndMaintenanceFavorite').select2('enable', true);

		$('#showFavoriteMethod').prop('disabled', false);

		$('#method-variates-section').hide();
		$('.method-selection-div').find('input,select').prop('disabled', false);

		changeAdvanceBreedingMethod();
	} else {
		$('#method-variates-section').show();
		$('.method-selection-div').find('input,select').prop('disabled', true);

		$('#methodIdAll').select2('enable', false);
		$('#methodIdFavorite').select2('enable', false);
		$('#methodIdDerivativeAndMaintenance').select2('enable', false);
		$('#methodIdDerivativeAndMaintenanceFavorite').select2('enable', false);

		if ($('#namingConvention').val() != 3) {
			$('#showFavoriteMethod').prop('disabled', 'disabled');
		}
		
		// we show the bulk and lines section
		$('.bulk-section').show();
		$('.lines-section').show();
		if ($('#methodVariateId').has('option').length === 0) {
			$('input[type=checkbox][name=methodChoice]').prop('checked', true);
			$('input[type=checkbox][name=methodChoice]').change();
			showErrorMessage('page-advance-modal-message', noMethodVariatesErrorTrial);

		} else {
			displaySectionsPerMethodVariateValues();
		}
	}
}

function displaySectionsPerMethodVariateValues() { //TODO ADVANCE
	'use strict';
	var id = $('#methodVariateId').val();
	if (id !== '') {
		$.ajax({
			url: '/Fieldbook/StudyManager/advance/study/checkMethodTypeMode/' + id,
			type: 'GET',
			cache: false,
			success: function(data) {
				if (data === 'LINE') {
					$('.lines-section').css('display', 'block');
				} else if (data === 'BULK') {
					$('.bulk-section').css('display', 'block');
				} else if (data === 'MIXED') {
					$('.lines-section').css('display', 'block');
					$('.bulk-section').css('display', 'block');
				}
			}
		});
	}
}

function lineMethod() { //TODO ADVANCE
	var $lineCheckBox = $('input[type=checkbox][name=lineChoice]');

	if (!$lineCheckBox.length) {
		return;
	}

	if ($lineCheckBox.is(':checked')) {
		$('#lineSelected').prop('disabled', false);
		$('#lineSelected').val(oldLineSelected);
		$('#line-variates-section').hide();
		$('.lines-per-plot-section').show();
	} else {
		$('#lineSelected').prop('disabled', 'disabled');
		oldLineSelected = $('#lineSelected').val();
		$('#lineSelected').val(1);
		$('#line-variates-section').show();
		$('.lines-per-plot-section').hide();
		if ($('#lineVariateId').has('option').length === 0) {
			$('input[type=checkbox][name=lineChoice]').prop('checked', true);
			$('input[type=checkbox][name=lineChoice]').change();
			showErrorMessage('page-advance-modal-message', noLineVariatesErrorTrial);
		}
	}
}

function validateAdvanceStudy() {

	// validate number of sample per plot
	var numberOfSamplePerPlot = $('#lineSelected').val();
	if (numberOfSamplePerPlot === '') {
		showInvalidInputMessage(msgSamplePlotError);
		return false;
	}
	if (!isInt(numberOfSamplePerPlot)) {
		showInvalidInputMessage(msgSamplePlotError);
		return false;
	}
	if (Number(numberOfSamplePerPlot) < 1
			|| Number(numberOfSamplePerPlot) > 1000) {
		showInvalidInputMessage(msgSamplePlotError);
		return false;
	}
	if ($('#harvestDate').val() === '') {
		showInvalidInputMessage(msgHarvestDateError);
		return false;
	}

	if ($('#namingConvention').val() === '3' && $('#breedingMethodId').val() === '0') {
		showInvalidInputMessage(msgMethodError);
		return false;
	}
	return true;
}

function showCorrectLocationCombo() { //TODO ADVANCE AND FIELDBOOK-COMMONS
	var isChecked = $('#showFavoriteLocation').is(':checked');
	// if show favorite location is checked, hide all field locations, else,
	// show only favorite locations
	if (isChecked) {

		if ($("#showBreedingLocationOnlyRadio").is(':checked')) {
			$('#s2id_harvestLocationIdBreedingFavorites').show();
			$('#s2id_harvestLocationIdFavorite').hide();
		} else {
			$('#s2id_harvestLocationIdBreedingFavorites').hide();
			$('#s2id_harvestLocationIdFavorite').show();
		}

		$('#s2id_harvestLocationIdAll').hide();
		$('#s2id_harvestLocationIdBreeding').hide();
		if ($('#' + getJquerySafeId('harvestLocationIdFavorite')).select2(
				'data') != null) {
			$('#' + getJquerySafeId('harvestLocationId')).val(
					$('#' + getJquerySafeId('harvestLocationIdFavorite'))
							.select2('data').id);
			$('#' + getJquerySafeId('harvestLocationName')).val(
					$('#' + getJquerySafeId('harvestLocationIdFavorite'))
							.select2('data').text);
			$('#' + getJquerySafeId('harvestLocationAbbreviation')).val(
					$('#' + getJquerySafeId('harvestLocationIdFavorite'))
							.select2('data').abbr);

		} else {
			$('#' + getJquerySafeId('harvestLocationId')).val(0);
			$('#' + getJquerySafeId('harvestLocationName')).val('');
			$('#' + getJquerySafeId('harvestLocationAbbreviation')).val('');
		}
	} else {
		$('#s2id_harvestLocationIdFavorite').hide();
		$('#s2id_harvestLocationIdBreedingFavorites').hide();

		if ($('#showAllLocationOnlyRadio').is(':checked')) {
			$('#s2id_harvestLocationIdAll').show();
			$('#s2id_harvestLocationIdBreeding').hide();
		} else {
			$('#s2id_harvestLocationIdBreeding').show();
			$('#s2id_harvestLocationIdAll').hide();
		}

        // harvestLocationIdAll is not null but it contains blank value so put AND condition
		if ($('#' + getJquerySafeId('harvestLocationIdAll')).select2('data') != null && $('#' + getJquerySafeId('harvestLocationIdAll')).select2('data').id != "" ) {
			$('#' + getJquerySafeId('harvestLocationId')).val($('#' + getJquerySafeId('harvestLocationIdAll')).select2('data').id);
			$('#' + getJquerySafeId('harvestLocationName')).val($('#' + getJquerySafeId('harvestLocationIdAll')).select2('data').text);
			$('#' + getJquerySafeId('harvestLocationAbbreviation')).val($('#' + getJquerySafeId('harvestLocationIdAll')).select2('data').abbr);
		}

	}
}

function setFavoriteMethodCheckbox(){//TODO ADVANCE
	if (methodSuggestionsDerivativeAndMaintenanceFavorite.length > 0) {
		$('#showFavoriteMethod').prop('checked', true);
	}
	else{
		$('#showFavoriteMethod').prop('checked', false);
	}
}

function showCorrectMethodCombo() {//TODO ADVANCE AND OTHERS
	var isChecked = $('#showFavoriteMethod').is(':checked');
	// if show favorite Method is checked, hide all field locations, else, show
	// only favorite methods
	var methodSelect = false;
	if ($('input[type=checkbox][name=methodChoice]:checked').val() === '1') {
		methodSelect = true;
	}

	// Hide everything then choose what to show
	$('#s2id_methodIdDerivativeAndMaintenanceFavorite').hide();
	$('#s2id_methodIdFavorite').hide();
	$('#s2id_methodIdAll').hide();
	$('#s2id_methodIdDerivativeAndMaintenance').hide();

	if (isChecked) {
		if ($('#showDerivativeAndMaintenanceRadio').is(':checked')) {
			$('#s2id_methodIdDerivativeAndMaintenanceFavorite').show();
		} else {
			$('#s2id_methodIdFavorite').show();
		}

	} else {
		if ($('#showDerivativeAndMaintenanceRadio').is(':checked')) {
			$('#s2id_methodIdDerivativeAndMaintenance').show();
		} else {
			$('#s2id_methodIdAll').show();
		}
	}
}

function getBreedingMethodRowIndex() { //TODO FIELDBOOK-COMMONS
	var rowIndex = -1;
	$.each($('.nurseryLevelSettings'), function(index, row) {
		var cvTermId = $($(row).find('.1st').find('.cvTermIds')).val();
		if (parseInt(cvTermId, 10) == parseInt(breedingMethodId, 10) || parseInt(cvTermId, 10) == parseInt($('#breedingMethodCode').val(), 10)) {
			rowIndex = getIndexFromName($($(row).find('.1st').find('.cvTermIds')).attr('name'));
		}
	});

	$.each($('.breedingMethodDetails'), function(index, row) {
		rowIndex = getIndexFromName($($(row).find('.1st').find('.cvTermIds')).attr('name'));
	});
	return rowIndex;
}

function getLocationRowIndex() { //TODO ADVANCE AND FIELDBOOK COMMONS
	var rowIndex = -1;
	$.each($('.nurseryLevelSettings'), function(index, row) {
		var cvTermId = $($(row).find('.1st').find('.cvTermIds')).val();
		if (parseInt(cvTermId, 10) == parseInt(locationId, 10)) {
			rowIndex = getIndexFromName($($(row).find('.1st').find('.cvTermIds')).attr('name'));
		}
	});

	return rowIndex;
}

function getIndexFromName(name) {
	return name.split('[')[1].split(']')[0];
}

//TODO FIELDBOOK COMMONS
function replacePossibleJsonValues(possibleValues, possibleValuesFavorite, allValues, allFavoriteValues, index) {
	'use strict';
	$('#possibleValuesJson' + index).text(JSON.stringify(possibleValues));
	$('#possibleValuesFavoriteJson' + index).text(JSON.stringify(possibleValuesFavorite));
	$('#allValuesJson' + index).text(JSON.stringify(allValues));
	$('#allFavoriteValuesJson' + index).text(JSON.stringify(allFavoriteValues));
}

//TODO FIELDBOOK COMMONS
function replacePossibleJsonValuesImportGermPlasm(possibleValues, possibleValuesFavorite, allValues, allFavoriteValues, index) {
	'use strict';
	$('#possibleValuesJson' + index).text(JSON.stringify(possibleValues));
	$('#possibleValuesFavoriteJson' + index).text(JSON.stringify(possibleValuesFavorite));
	$('#allPossibleValuesJson' + index).text(JSON.stringify(allValues));
	$('#allPossibleValuesFavoriteJson' + index).text(JSON.stringify(allFavoriteValues));
}

//TODO FIELDBOOK COMMONS
function setComboValues(suggestions_obj, id, name) {
	var dataVal = {
		id: '',
		text: '',
		description: ''
	}; // default value
	if (id != '') {
		var count = 0;
		// find the matching value in the array given
		for (count = 0; count < suggestions_obj.length; count++) {
			if (suggestions_obj[count].id == id) {
				dataVal = suggestions_obj[count];
				break;
			}
		}
	}
	// set the selected value of the combo
	$('#' + name).select2('data', dataVal);
}

/* IMPORT STUDY */
function toggleDropdownImportStudy(allCheckId, comboId, favoriteCheckId, suffix, isLocation) {
	var possibleValues;
	var showFavorite = $('#' + favoriteCheckId).is(':checked');
	var showAll = $('#' + allCheckId).is(':checked');
	var selectedVal = '';

	// get previously selected value
	if ($('#' + comboId).select2('data')) {
		selectedVal = $('#' + comboId).select2('data').id;
	}

	// reset select2 combo
	initializePossibleValuesCombo([], '#' + comboId, isLocation, null);

	// get possible values based on checkbox
	if (showFavorite) {
		if (showAll) {
			possibleValues = $('#allPossibleValuesFavoriteJson' + suffix).text();
		} else {
			possibleValues = $('#possibleValuesFavoriteJson' + suffix).text();
		}
		$('#' + comboId).parent().find('.selectedValue').val(selectedVal);
		selectedVal = $('#' + comboId).parent().find('.selectedValueFave').val();
	} else if (showAll) {
		possibleValues = $('#allPossibleValuesJson' + suffix).text();
		$('#' + comboId).parent().find('.selectedValueFave').val(selectedVal);
		selectedVal = $('#' + comboId).parent().find('.selectedValue').val();
	} else {
		possibleValues = $('#possibleValuesJson' + suffix).text();
		$($('#' + comboId).parent().find('.selectedValueFave')).val(selectedVal);
		selectedVal = $($('#' + comboId).parent().find('.selectedValue')).val();
	}

	// recreate select2 combo
	initializePossibleValuesCombo($.parseJSON(possibleValues), '#' + comboId,
			showFavorite ? false : isLocation, selectedVal);

}

function sortByKey(array, key) {
	return array.sort(function(a, b) {
		var x = a[key].toLowerCase();
		var y = b[key].toLowerCase();
		return ((x < y) ? -1 : ((x > y) ? 1 : 0));
	});
}

function initializePossibleValuesCombo(possibleValues, name, isLocation,
		defaultValue) {
	var possibleValues_obj = [],
		counter = 0,
		newPossibleValues = [];
	var defaultJsonVal = null;
	var isBreedingMethodSetting = $(name).parent().next().children('.breeding-method-tooltip').length > 0;

	$.each(
		possibleValues,
		function(index, value) {
			var jsonVal;
			if (isBreedingMethodSetting && value.id !== undefined) {
				jsonVal = {
					'id': value.key,
					'text': value.description,
					'description': value.name
				};
			} else if (value.id !== undefined) {
				jsonVal = {
					'id': value.key,
					'text': value.description
				};
			} else if (value.locid !== undefined) {
				var locNameDisplay = value.lname;
				if (value.labbr != null && value.labbr != '') {
					locNameDisplay  += ' - (' + value.labbr + ')';
				}
				jsonVal = {
					'id': value.locid,
					'text': locNameDisplay
				};
			} else {
				jsonVal = {
					'id': value.mid,
					'text': value.mname + (value.mcode !== undefined ? ' - ' + value.mcode : ''),
					'description': value.mdesc
				};
			}

			possibleValues_obj.push(jsonVal);
			if (defaultValue !== null
					&& defaultValue !== ''
					&& ((defaultValue === value.key
							|| defaultValue == value.locid || defaultValue == value.mid) || (defaultValue == value.name
							|| defaultValue == value.lname || defaultValue == value.mname))) {
				defaultJsonVal = jsonVal;
			}

		});

	if (isLocation) {
		$(name).select2(
				{
					minimumResultsForSearch: (possibleValues_obj != null && possibleValues_obj.length != 0) ? 20 : -1,
					query: function(query) {
						var data = {
							results: possibleValues_obj
						};
						// return the array that matches
						data.results = $.grep(data.results, function(item,
								index) {
							return ($.fn.select2.defaults.matcher(query.term,
									item.text));

						});
						query.callback(data);
					}
				});

        // Set default harvest location id and name
        if (defaultJsonVal != null) {
            $('#' + getJquerySafeId('harvestLocationId')).val(defaultJsonVal.id);
            $('#' + getJquerySafeId('harvestLocationName')).val(defaultJsonVal.text);
        }
	} else if ($(name).parent().next().children('.breeding-method-tooltip').length > 0) {
		$(name).select2(
				{
					minimumResultsForSearch: (possibleValues_obj != null && possibleValues_obj.length != 0) ? 20: -1,
					query: function(query) {
						var data = {
							results: possibleValues_obj
						};
						// return the array that matches
						data.results = $.grep(data.results, function(item,
								index) {
							return ($.fn.select2.defaults.matcher(query.term,
									item.text));

						});
						query.callback(data);
					}
				}).on('change', function() {
					if ($('.breeding-method-tooltip')) {
						$('.breeding-method-tooltip').attr('title', $(name).select2('data').description);
						$('.help-tooltip-study').tooltip('destroy');
						$('.help-tooltip-study').tooltip();
						if ($(name).select2('data') != null && $(name).select2('data').id === '0') {
							$('.breeding-method-tooltip').addClass('fbk-hide');
						} else {
							$('.breeding-method-tooltip').removeClass('fbk-hide');
						}
					}
				});
	} else {
		var minResults = (possibleValues_obj.length > 0) ? 20: -1;

		$(name).select2(
				{
					minimumResultsForSearch: minResults,
					query: function(query) {
						var data = {
							results: possibleValues_obj
						};
						// return the array that matches
						data.results = $.grep(data.results, function(item,
								index) {
							return ($.fn.select2.defaults.matcher(query.term,
									item.text));

						});
						query.callback(data);
					}
				});
	}
	if (defaultJsonVal != null) {
		$(name).select2('data', defaultJsonVal).trigger('change');
	}


	// override the select2 open event to hide the selected item in the dropdown list
	$(name).off("select2-open");
	$(name).on("select2-open", function () {

			// get values of selected option
			var values = $(this).val();
			// get the pop up selection
			var popupSelection = $('.select2-result.select2-highlighted');

			if (values !== null && values !== '') {
				// hide the selected value
				popupSelection.hide();

			} else {
				// show all the selection value
				popupSelection.show();
			}
		}
	);
}

function initializeDateAndSliderInputs() {
	if ($('.date-input').length > 0) {
		$('.date-input').placeholder().each(function() {
			$(this).datepicker({
				'format': 'yyyy-mm-dd'
			}).on('changeDate', function(ev) {
				$(this).datepicker('hide');
			});
		});
	}
	if ($('.datepicker img').length > 0) {
		$('.datepicker img').on('click', function() {
			$(this).parent().parent().find('.date-input').datepicker('show');
		});
	}
	if ($('.spinner-input').length > 0) {

		$('.spinner-input').each(
				function() {
					var currentVal = $(this).val() == '' ? parseFloat($(this)
							.data('min')) : parseFloat($(this).val());
					$(this).spinedit({
						minimum: parseFloat($(this).data('min')),
						maximum: parseFloat($(this).data('max')),
						step: parseFloat($(this).data('step')),
						value: currentVal,
						numberOfDecimals: 4
					});
				});
	}
}

function recreateModalMethodCombo(comboName, comboFaveCBoxName, url) {
	var selectedMethodAll = $('#methodIdAll').val();
	var selectedMethodDerivativeAndMaintenance = $('#methodIdDerivativeAndMaintenance').val();
	var selectedMethodDerivativeAndMaintenanceFavorite = $('#methodIdDerivativeAndMaintenanceFavorite').val();
	var selectedMethodFavorite = $('#methodIdFavorite').val();

	$.ajax({
		url: url || '/Fieldbook/breedingMethod/getBreedingMethods',
		type: 'GET',
		cache: false,
		data: '',
		async: false,
		success: function(data) {
			if (data.success == '1') {
				if (selectedMethodAll != null) {
					// recreate the select2 combos to get updated list of
					// methods
				
					if (data.favoriteNonGenerativeMethods && data.favoriteNonGenerativeMethods.length > 0) {
						$('#showFavoriteMethod').prop('checked', true);
					} else {
						$('#showFavoriteMethod').prop('checked', false);
					}

					recreateMethodComboAfterClose('methodIdAll', data.allMethods);
					recreateMethodComboAfterClose('methodIdFavorite', data.favoriteMethods);
					recreateMethodComboAfterClose('methodIdDerivativeAndMaintenance', data.allNonGenerativeMethods);
					recreateMethodComboAfterClose('methodIdDerivativeAndMaintenanceFavorite', data.favoriteNonGenerativeMethods);
					showCorrectMethodCombo();
					// set previously selected value of method
					if ($('#showFavoriteMethod').prop('checked')) {
						if ($('#showDerivativeAndMaintenanceRadio').prop('checked')) {
							setComboValues(methodSuggestionsDerivativeAndMaintenanceFavorites_obj, selectedMethodDerivativeAndMaintenanceFavorite, 'methodIdDerivativeAndMaintenanceFavorite');
						} else {
							setComboValues(methodSuggestionsFav_obj, selectedMethodFavorite, 'methodIdFavorite');
						}
					} else if ($('#showDerivativeAndMaintenanceRadio').prop('checked')) {
						setComboValues(methodSuggestionsDerivativeAndMaintenance_obj, selectedMethodDerivativeAndMaintenance, 'methodIdAll');
					} else {
						setComboValues(methodSuggestions_obj, selectedMethodAll, 'methodIdAll');
					}
				} else {
					var selectedVal = null;
					// get index of breeding method row
					var index = getBreedingMethodRowIndex();

					if ($('#' + getJquerySafeId(comboName)).select2('data')) {
						selectedVal = $('#' + getJquerySafeId(comboName))
								.select2('data').id;
					}
					// recreate select2 of breeding method
					initializePossibleValuesCombo([], '#'
							+ getJquerySafeId(comboName), false, selectedVal);

					// update values of combo
					if ($('#' + getJquerySafeId(comboFaveCBoxName)).is(
							':checked')) {
						initializePossibleValuesCombo($
								.parseJSON(data.favoriteNonGenerativeMethods), '#'
								+ getJquerySafeId(comboName), false,
								selectedVal);
					} else {
						initializePossibleValuesCombo(data.allNonGenerativeMethods, '#'
								+ getJquerySafeId(comboName), false,
								selectedVal);
					}

					if (index > -1) {
						replacePossibleJsonValues(data.allNonGenerativeMethods, data.favoriteNonGenerativeMethods, data.allMethods,
							data.favoriteMethods, index);
					}
				}
			} else {
				showErrorMessage('page-message', data.errorMessage);
			}

		}
	});
}

function plotMethod() {
	var $plotCheckBox = $('input[type=checkbox][name=allPlotsChoice]');

	if (!$plotCheckBox.length) {
		return;
	}

	if ($plotCheckBox.is(':checked')) {
		$('#plot-variates-section').hide();
	} else {
		$('#plot-variates-section').show();
		if ($('#plotVariateId').has('option').length === 0) {
			$('input[type=checkbox][name=allPlotsChoice]')
					.prop('checked', true);
			$('input[type=checkbox][name=allPlotsChoice]').change();
			showErrorMessage('page-advance-modal-message', noLineVariatesErrorTrial);
		}
	}
}

function displaySaveSuccessMessage(idDomSelector, messageToDisplay) {
	'use strict';
	createSuccessNotification(successMsgHeader, messageToDisplay);
}


// FIXME Should not be using global variables or functions
/*
 * global lastDraggedChecksList, Spinner,
 * moveToTopScreen
 */
/* global getJquerySafeId */

function resetDesigConfirmationFields() {
	'use strict';
	// reset dropdowns and fields
	$('#importLocationId').select2('data', null);
	$('#importMethodId').select2('data', null);
	$('#nameType').select2('data', {'id': $('#nameType option:first').val(), 'text':$('#nameType option:first').html()});
	$('#importDate').val('');
	$('#confirmation-page-message').html('');
}

function validateGermplasmInput(importDate, importLocationId, importMethodId) {
	'use strict';
	if ($('#import-action-type').val() === '2' || $('#import-action-type').val() === '1') {
		if (importDate === '') {
			showInvalidInputMessage(importDateRequired);
			return false;
		}else if (importLocationId === '' || importLocationId === null) {
			showInvalidInputMessage(importLocationRequired);
			return false;
		}else if ((importMethodId === '' || importMethodId === null) && $('#import-action-type').val() === '2') {
			showInvalidInputMessage(importMethodRequired);
			return false;
		}
	}
	return true;
}

function addFakeCheckTable() {
	'use strict';
	if ($('.germplasm-list-items tbody tr').length > 0 && $('.check-germplasm-list-items tbody tr').length == 0 && $('#check-germplasm-list .fake-check-germplasm-list-items tbody tr').length == 0) {
		// we add the fake table
		$('.fake-check-germplasm-list-items').clone().removeClass('fbk-hide').appendTo('#check-germplasm-list');
	}else if ($('.germplasm-list-items tbody tr').length === 0 && $('#check-germplasm-list .fake-check-germplasm-list-items tbody tr').length == 1) {
		// we remove if there are no nursery check and the selected check is
		// fake
		$('#check-germplasm-list .fake-check-germplasm-list-items').remove();
	}
}

function discardImportedData() {
	$('#discardImportDataConfirmation').modal({
		backdrop: 'static',
		keyboard: true
	});
}

