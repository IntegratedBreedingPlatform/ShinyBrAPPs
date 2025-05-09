/*global showErrorMessage, createErrorNotification, crossingImportErrorHeader, isInt, crossingExportErrorHeader, invalidImportedFile,
getJquerySafeId, SaveAdvanceList, BreedingMethodsFunctions */
var ImportCrosses = {
	CROSSES_URL: '/Fieldbook/crosses',
	showFavoriteMethodsOnly: true,
	showFavoriteLocationsOnly: true,
	preservePlotDuplicates: false,
	isFileCrossesImport: true,
	hasHybridMethod: false,
	hybridMethods: null,
	showPopup: function() {
		'use strict';
		$('#fileupload-import-crosses').val('');
		$('.import-crosses-section .modal').modal({ backdrop: 'static', keyboard: true });
		$('.import-crosses-section .modal .fileupload-exists').click();
		ImportCrosses.showFavoriteMethodsOnly = true;
		ImportCrosses.showFavoriteLocationsOnly = true;
	},

	doSubmitImport: function() {
		'use strict';

		if ($('#fileupload-import-crosses').val() === '') {
			showErrorMessage('', $.fieldbookMessages.errorNoFileSelectedForImport);
			return false;
		}

		ImportCrosses.submitImport($('.import-crosses-section')).done(function(resp) {

			if (!resp.isSuccess) {
				createErrorNotification(crossingImportErrorHeader, resp.error.join('<br/>'));
				return;
			}

			if (resp.warnings) {
				createWarningNotification(warningMsgHeader, resp.warnings.join('<br/>'), 10000);
			}

			ImportCrosses.preservePlotDuplicates = false;
			$('.import-crosses-section .modal').modal('hide');
			$('#openCrossesListModal').data('hasPlotDuplicate', resp.hasPlotDuplicate);
			// show review crosses page

			ImportCrosses.isFileCrossesImport = true;
			createdCrossesListId = null;

			ImportCrosses.hasHybridMethod = resp.hasHybridMethod;

			$('#crossSetBreedingMethodModal').addClass('import-crosses-from-file');

			if (resp.isChoosingListOwnerNeeded) {
				$('#chooseListOwner').modal({ backdrop: 'static', keyboard: true });
				$('#chooseListOwner').addClass('import-crosses-from-file');

				$('#goBackToImportFileCrossesButton').off('click');
				$('#goBackToImportFileCrossesButton').on('click', function() {
					ImportCrosses.goBackToPage('#chooseListOwner', '.import-crosses-section .modal');
				});

				$('#chooseListOwnerNextButton').on('click', function() {
					if (ImportCrosses.isFileCrossesImport) {
						$('#crossSetBreedingMethodModal').addClass('import-crosses-from-file');
					}
					$('#chooseListOwner').modal('hide');
					setTimeout(ImportCrosses.showPlotDuplicateConfirmation, 500);

				});

			} else {
				setTimeout(ImportCrosses.showPlotDuplicateConfirmation, 500);
			}

		});

	},

	openBreedingModal: function() {
		'use strict';
		var crossSettingsPopupModal = $('#crossSetBreedingMethodModal');
		crossSettingsPopupModal.modal({ backdrop: 'static', keyboard: true });

		if(!ImportCrosses.hasHybridMethod) $("#applyGroupingOptionDiv").hide();

		$("#breedingMethodSelectionDiv :input").attr("disabled", true);
		$('#breedingMethodDropdown').select2('val', null);
		$('#breedingMethodDropdown').on('change', ImportCrosses.retrieveHybridMethods);
		$("#showFavoritesOnlyCheckbox").prop('checked', true);
		$("#showBreedingMethodOnlyRadio").prop('checked', true);

		if (createdCrossesListId == null) {
			$('#selectMethodInImportFile').prop('checked',true);
			$('#selectUseParentalStatus').prop('checked',false);

		} else {
			$('#selectMethodInImportFile').prop('checked',false);
			$('#selectUseParentalStatus').prop('checked',true);
		}

		$('#selectMethodForAllCrosses').off('change');
		$('#selectMethodForAllCrosses').on('change', ImportCrosses.enableDisableBreedingMethodDropdown)

		$('#selectMethodInImportFile').off('change');
		$('#selectMethodInImportFile').on('change', ImportCrosses.enableDisableBreedingMethodDropdown)

		$('#selectUseParentalStatus').off('change');
		$('#selectUseParentalStatus').on('change', ImportCrosses.enableDisableBreedingMethodDropdown)

		$('#setNamingNextButton').off('click');
		$('#setNamingNextButton').click(function () {
			if (ImportCrosses.isBreedingMethodSelectedValid()) {
				$('#crossSetBreedingMethodModal').modal('hide');
				setTimeout(ImportCrosses.showImportSettingsPopup, 500);
			} else {
				showErrorMessage('', $.fieldbookMessages.errorMethodMissing);
			}
		});

		$('#crossSettingsModal').one('show.bs.modal', function() {
			ImportCrosses.resetCrossSettingsModal();
		});

		$('#goBackToImportCrossesButton').off('click');
		$('#goBackToImportCrossesButton').on('click', function() {
			ImportCrosses.goBackToPage('#crossSetBreedingMethodModal', '.import-crosses-section .modal');
		});

		BreedingMethodsFunctions.processMethodDropdownAndFavoritesCheckbox('breedingMethodDropdown', 'showFavoritesOnlyCheckbox',
			'showAllMethodOnlyRadio', 'showBreedingMethodOnlyRadio');

	},

	retrieveHybridMethods : function () {
		if(ImportCrosses.hybridMethods === null){
			$.ajax({
				url: ImportCrosses.CROSSES_URL + '/getHybridMethods',
				type: 'GET',
				cache: false,
				success: function(data) {
					ImportCrosses.hybridMethods = data;
				}
			}).done(ImportCrosses.showOrHideApplyGroupingOptionDiv);
		} else {
			ImportCrosses.showOrHideApplyGroupingOptionDiv();
		}
	},

	retrieveLocationIdFromFirstEnviroment: function () {
		return $.ajax({
			url: ImportCrosses.CROSSES_URL + '/getLocationIdFromFirstEnviroment',
			type: 'GET',
			cache: false,
			async: false,
			success: function (data) {
			},
			error: function (jqXHR, textStatus, errorThrown) {
				console.log('The following error occurred: ' + textStatus, errorThrown);
			},
			complete: function () {
			}
		});
	},

	showOrHideApplyGroupingOptionDiv : function () {
		if(!ImportCrosses.hybridMethods.includes(parseInt($('#breedingMethodDropdown').select2('val')))) {
			$("#applyGroupingOptionDiv").hide();
		} else {
			$("#applyGroupingOptionDiv").show();
		}
	},

	resetCrossSettingsModal: function () {
		$('#crossPrefix').val('');
		$('#sequenceNumberDigits').select2('val', '');
		$('#crossSuffix').val('');
		$('input:radio[name=hasPrefixSpace][value=' + false + ']').prop('checked', true);
		$('input:radio[name=hasSuffixSpace][value=' +false + ']').prop('checked', true);
		$('input:radio[name=manualNamingSettings][value=' +false + ']').prop('checked', true);
		$('#manualNamingSettingsPanel').addClass('fbk-hide');
		$('input:radio[name=hasParentageDesignationName][value=' +false + ']').prop('checked', true);
		$('#parentageDesignationSeparator').val('/');
		$('#startingSequenceNumber').val('');
		$('#locationDropdown').select2('val', '');
		$('#importNextSequenceName').text('');
		$('#presetName').val('');
		$('#checkExistingCrosses').prop('checked', false);
		$('#showOnlyRecordsWithAlerts').prop('checked', false);
	},

	hasPlotDuplicate: function() {
		'use strict';
		if ($('#openCrossesListModal').data('hasPlotDuplicate') === true) {
			return true;
		}
		return false;
	},

	openCrossesList: function(createdCrossesListId) {
		'use strict';


		$('#openCrossesListModal').one('shown.bs.modal', function() {
			// After the modal window is shown, make sure that the table header is properly adjusted.
			$('#preview-crosses-table').resize();
		}).modal({ backdrop: 'static', keyboard: true });

		if (ImportCrosses.isFileCrossesImport) {
			$('#openCrossesListModal').addClass('import-crosses-from-file');
		}

		$('#openCrossesListModal').on('hidden.bs.modal', function() {
			$('#openCrossesListModal').removeClass('import-crosses-from-file');
		});

		$('#crossSettingsModal').on('hidden.bs.modal', function() {
			//we should clear the form and form fields on closing as we are going to reuse it later
			//TODO clear other fields as well, some of them need to set the default value back
			$('#crossSettingsModal').removeClass('import-crosses-from-file');
		});

		$('#openCrossListNextButton').off('click');
		$('#openCrossListNextButton').on('click', function() {
			$('#openCrossesListModal').modal('hide');
			$('#settingsNextButton').off('click');
			// delete temporary list created from Design Crosses
			ImportCrosses.deleteCrossList(createdCrossesListId)
				.done(ImportCrosses.openSaveListModal)
				.fail(function () {
					showErrorMessage('', 'Could not delete cross list');
				});
		});

		$('#goBackToNamingModal').off('click');
		$('#goBackToNamingModal').on('click', function() {
			ImportCrosses.goBackToPage('#openCrossesListModal', '#crossSettingsModal');
		});

		return ImportCrosses.getImportedCrossesTable(createdCrossesListId).done(function(response) {
			if (response.isSuccess === 0) {
				showErrorMessage('', response.error);
				return;
			}
			var checkExistingCrosses = $('#checkExistingCrosses').is(':checked');
			new  BMS.Fieldbook.PreviewCrossesDataTable('#preview-crosses-table', response.listDataTable, response.tableHeaderList,
				response.isImport, checkExistingCrosses);
		}).fail(function (jqXHR, textStatus) {
			showErrorMessage('', textStatus);
		});
	},

	viewExistingCrosses: function(femaleGID, maleGIDs, cross, breedingMethodId, gid) {
		$('#openCrossesListModal').modal('hide');
		$('#existingCrossesModal').one('shown.bs.modal', function() {
			$('#existingCrossesModal .modal-title').html(cross);
			$('#existingCrossesModal .existing-cross-label').html('<i>Cross <b>' + cross + '</b> already exists in the database.</i>');
			// After the modal window is shown, make sure that the table header is properly adjusted.
			$('#existing-crosses-table').resize();
		}).modal({ backdrop: 'static', keyboard: true });

		$('#existingCrossesModal').on('hidden.bs.modal', function () {
			$('#existingCrossesModal').modal ('hide');
			$('#openCrossesListModal').one('shown.bs.modal', function() {
				// After the modal window is shown, make sure that the table header is properly adjusted.
				$('#preview-crosses-table').resize();
			}).modal({ backdrop: 'static', keyboard: true });
		});

		return ImportCrosses.getExistingCrossesTable(femaleGID, maleGIDs, breedingMethodId, gid).done(function(response) {
			if (response.isSuccess === 0) {
				showErrorMessage('', response.error);
				return;
			}
			new  BMS.Fieldbook.ExistingCrossesDataTable('#existing-crosses-table', response.listDataTable, response.tableHeaderList);
		}).fail(function (jqXHR, textStatus) {
			showErrorMessage('', textStatus);
		});

	},

	deleteCrossList: function (createdCrossesListId) {
		if (!createdCrossesListId) {
			return $.Deferred().resolve();
		}
		return $.ajax({
			url: ImportCrosses.CROSSES_URL + '/deleteCrossList/' + createdCrossesListId,
			type: 'DELETE',
			cache: false,
			global: false
		});
	},

	goBackToPage: function(hiddenModalSelector, shownModalSelector) {
		'use strict';
		$(hiddenModalSelector).modal('hide');
		$(shownModalSelector).one('shown.bs.modal', function() {
			// The bootstrap modal intermittently adds extra padding from the left that makes the layout of dialog content inconsistent when loading.
			// We add this line to prevent that inconsistency with padding.
			$(shownModalSelector).addClass('remove-excess-padding');
		}).modal({ backdrop: 'static', keyboard: true });
	},

	getImportedCrossesTable: function(createdCrossesListId) {
		'use strict';
		var checkExistingCrosses = $('#checkExistingCrosses').is(':checked');
		var crossesURL = ImportCrosses.CROSSES_URL + '/getImportedCrossesList' + '/' + checkExistingCrosses + '/' + (createdCrossesListId &&
		createdCrossesListId.length > 0 ? createdCrossesListId : '');
		return $.ajax(
			{
				url: crossesURL,
				type: 'GET',
				cache: false,
				global: false
			});
	},

	getExistingCrossesTable: function(femaleGID, maleGIDs, breedingMethodId, gid) {
		'use strict';
		var crossesURL = ImportCrosses.CROSSES_URL + '/getExistingCrossesList' + '/' + femaleGID + '/' + maleGIDs + '/' + gid;
		return $.ajax(
			{
				url: crossesURL,
				type: 'GET',
				cache: false,
				global: false
			});
	},

	submitImport: function($importCrossesForm) {
		'use strict';
		var deferred = $.Deferred();
		$importCrossesForm.ajaxForm({
			dataType: 'json',
			success: function(response) {
				deferred.resolve(response);
			},
			error: function(response) {
				createErrorNotification(crossingImportErrorHeader, invalidImportedFile);
				deferred.reject(response);
			}
		}).submit();

		return deferred.promise();
	},


	showPlotDuplicateConfirmation: function() {
		'use strict';
		if (ImportCrosses.hasPlotDuplicate()) {
			/** Functionality temporarily suppress by the v4 public release.  See issue: BMS-3514 **/
			/*	//show the confirmation now
				$('#duplicate-crosses-modal input[type=checkbox]').prop('checked', ImportCrosses.preservePlotDuplicates);

				$('#duplicate-crosses-modal').modal({ backdrop: 'static', keyboard: true });

				$('#continue-duplicate-crosses').off('click');
				$('#continue-duplicate-crosses').on('click', function() {
					//get the value of the checkbox
					ImportCrosses.preservePlotDuplicates = $('#duplicate-crosses-modal input[type=checkbox]').is(':checked');
					$('#duplicate-crosses-modal').modal('hide');
					setTimeout(ImportCrosses.showImportSettingsPopup, 500);
				});
				*/
			/** End Functionality temporarily suppress **/
			/** Palliative for BMS-3514 **/
			ImportCrosses.preservePlotDuplicates = true;
			ImportCrosses.openBreedingModal();
			/** End Palliative **/
		} else {
			ImportCrosses.openBreedingModal();
		}
	},

	showImportSettingsPopup: function() {
		'use strict';
		var crossSettingsPopupModal = $('#crossSettingsModal');
		crossSettingsPopupModal.modal({ backdrop: 'static', keyboard: true });
		ImportCrosses.retrieveLocationIdFromFirstEnviroment().done(function (locationId) {
			LocationsFunctions.processLocationDropdownAndFavoritesCheckbox('locationDropdown', 'locationFavoritesOnlyCheckbox',
				'showAllLocationOnlyRadio', 'showBreedingLocationOnlyRadio', undefined, undefined, locationId);
		});

		ImportCrosses.processImportSettingsDropdown('presetSettingsDropdown', 'loadSettingsCheckbox');

		$('#presetSettingsDelete').off('click');
		$('#presetSettingsDelete').on('click', function () {

			var data = $('#presetSettingsDropdown').select2('data');
			if (!(data && data.programPresetId)) {
				return;
			}

			crossSettingsPopupModal.modal('hide');
			crossSettingsPopupModal.data('open', '1');

			var deleteModalElm = $('#fbk-delete-import-settings-confirm');
			$('#fbk-delete-import-settings-confirm .yes').on('click', function() {
				deleteModalElm.modal('hide');
				deleteModalElm.data('open', '1');
				setTimeout(function () {
					crossSettingsPopupModal.modal({ backdrop: 'static', keyboard: true })
					ImportCrosses.onDeleteSettingOk(data);
				}, 500)
			});

			$('#fbk-delete-import-settings-confirm .no').on('click', function() {
				deleteModalElm.modal('hide');
				deleteModalElm.data('open', '1');
				setTimeout(function () {
					crossSettingsPopupModal.modal({ backdrop: 'static', keyboard: true })
				}, 500)
			});
			deleteModalElm.modal({ backdrop: 'static', keyboard: true });
		});




		ImportCrosses.updateSampleParentageDesignation();

		$('.cross-import-name-setting').off('change');
		$('.cross-import-name-setting').on('change', ImportCrosses.updateDisplayedSequenceNameValue);

		$('#parentageDesignationSeparator').off('change');
		$('#parentageDesignationSeparator').on('change', ImportCrosses.updateSampleParentageDesignation);

		ImportCrosses.populateHarvestMonthDropdown('harvestMonthDropdown');
		ImportCrosses.populateHarvestYearDropdown('harvestYearDropdown');

		$('#settingsNextButton').off('click');
		$('#settingsNextButton').click(function() {
			var valid = true;
			var settingData = ImportCrosses.constructSettingsObjectFromForm();
			if (!ImportCrosses.isCrossImportSettingsValid(settingData)) {
				valid = false;
			}
			if (valid) {
				ImportCrosses.retrieveNextNameInSequence(function(data){
					if (data.success === '1') {
						ImportCrosses.showCrossListPopup(crossSettingsPopupModal);
					} else {
						showErrorMessage('', data.error);
					}
				}, function(){ showErrorMessage('', $.fieldbookMessages.errorNoNextNameInSequence)} );
			}

		});

		$('#goBackToSelectBreedingMethodModal').off('click');
		$('#goBackToSelectBreedingMethodModal').on('click', function() {
			ImportCrosses.showFavoriteMethodsOnly = $('#showFavoritesOnlyCheckbox').is(':checked');
			ImportCrosses.showFavoriteLoationsOnly = $('#locationFavoritesOnlyCheckbox').is(':checked');
			ImportCrosses.showAllLocationOnly = $('#showAllLocationOnlyRadio').is(':checked');
			ImportCrosses.showBreedingLocationOnly = $('#showBreedingLocationOnlyRadio').is(':checked');
			ImportCrosses.goBackToPage('#crossSettingsModal', '#crossSetBreedingMethodModal');
		});
	},

	onDeleteSettingOk: function(data) {
		ImportCrosses.deleteImportSettings(data.programPresetId)
			.done(function () {
				showSuccessfulMessage('', crossingSettingsDeleted);
				ImportCrosses.processImportSettingsDropdown('presetSettingsDropdown', 'loadSettingsCheckbox');
			})
			.fail(function () {
				showErrorMessage('', crossingSettingsDeleteFailed);
			});
	},

	showCrossListPopup : function(crossSettingsPopupModal) {
		$(crossSettingsPopupModal).modal('hide');
		setTimeout(function () {
			ImportCrosses.submitCrossImportSettings().then(function () {
				// createdCrossesListId (global) will be null for import
				return ImportCrosses.openCrossesList(createdCrossesListId);
			});
		}, 500);
	},

	enableDisableBreedingMethodDropdown : function() {
		var radioValue = $('#selectMethodForAllCrosses').prop('checked');
		if (radioValue) {
			$("#breedingMethodSelectionDiv :input").attr("disabled", false);
		} else {
			$("#breedingMethodSelectionDiv :input").attr("disabled", true);
			$('#breedingMethodDropdown').select2('val', null);
		}

		if ($('#selectMethodInImportFile').prop('checked') && ImportCrosses.hasHybridMethod) {
			$("#applyGroupingOptionDiv").show();
		} else {
			$("#applyGroupingOptionDiv").hide();
		}

	},

	validateStartingSequenceNumber: function(value) {
		'use strict';
		if (value !== null && value !== '' && (value.indexOf('.') >= 0 || !isInt(value))) {
			createErrorNotification(invalidInputMsgHeader, invalidStartingNumberErrorMsg);
			return false;
		}
		return true;
	},

	updateSampleParentageDesignation: function() {
		'use strict';
		var value = $('#parentageDesignationSeparator').val();
		$('#sampleParentageDesignation').text('FEMALE-123' + value + 'MALE-456');
	},

	processImportSettingsDropdown: function(dropdownID, useSettingsCheckboxID) {
		'use strict';
		ImportCrosses.retrieveAvailableImportSettings().done(function(settingList) {
			ImportCrosses.createAvailableImportSettingsDropdown(dropdownID, settingList);

			$('#' + getJquerySafeId(dropdownID)).on('change', function() {
				ImportCrosses.triggerImportSettingUpdate(settingList, dropdownID, useSettingsCheckboxID);
				// update the displayed sequence name value so that it makes use of the possibly new settings
				ImportCrosses.updateDisplayedSequenceNameValue();
				ImportCrosses.updateSampleParentageDesignation();
			});

			$('#' + getJquerySafeId(useSettingsCheckboxID)).on('change', function() {
				ImportCrosses.triggerImportSettingUpdate(settingList, dropdownID, useSettingsCheckboxID);

				// update the displayed sequence name value so that it makes use of the possibly new settings
				ImportCrosses.updateDisplayedSequenceNameValue();
				ImportCrosses.updateSampleParentageDesignation();
			});
		}).fail(function() {
			//TODO Process errors
		});
	},

	triggerImportSettingUpdate: function(settingList, dropdownID, useSettingsCheckboxID) {
		'use strict';
		if ($('#' + getJquerySafeId(useSettingsCheckboxID)).is(':checked')) {
			var currentSelectedItem = $('#' + dropdownID).select2('val');

			$.each(settingList, function(index, setting) {
				if (setting.name === currentSelectedItem) {
					ImportCrosses.updateImportSettingsFromSavedSetting(setting);
				}
			});
		}
	},

	updateImportSettingsFromSavedSetting: function(setting) {
		'use strict';
		$('#presetName').val(setting.name);
		$('#breedingMethodDropdown').select2('val', setting.breedingMethodID);
		$('#useSelectedMethodCheckbox').prop('checked', !setting.basedOnStatusOfParentalLines);

		$('#crossPrefix').val(setting.crossPrefix);
		$('#crossSuffix').val(setting.crossSuffix);
		$('input:radio[name=hasPrefixSpace][value=' + setting.hasPrefixSpace + ']').prop('checked', true);
		$('input:radio[name=hasSuffixSpace][value=' + setting.hasSuffixSpace + ']').prop('checked', true);
		$('input:radio[name=hasParentageDesignationName][value=' + setting.hasParentageDesignationName + ']').prop('checked', true);
		$('#sequenceNumberDigits').select2('val', setting.sequenceNumberDigits);
		$('#parentageDesignationSeparator').val(setting.parentageDesignationSeparator);
		$('#startingSequenceNumber').val(setting.startingSequenceNumber);
		$('#locationDropdown').select2('val', setting.locationID);
	},

	openBreedingMethodsModal: function() {
		'use strict';
		var crossSettingsPopupModal = $('#crossSetBreedingMethodModal');
		crossSettingsPopupModal.modal('hide');
		crossSettingsPopupModal.data('open', '1');

		BreedingMethodsFunctions.openMethodsModal();

		$('#manageMethodModal').one('hidden.bs.modal', function () {
			$('#manageMethodModal').modal ('hide');
			$('#crossSetBreedingMethodModal').modal({ backdrop: 'static', keyboard: true });

			BreedingMethodsFunctions.processMethodDropdownAndFavoritesCheckbox('breedingMethodDropdown', 'showFavoritesOnlyCheckbox',
				'showAllMethodOnlyRadio', 'showBreedingMethodOnlyRadio');
		});
	},

	openLocationsModal: function() {
		'use strict';
		var crossSettingsPopupModal = $('#crossSettingsModal');
		crossSettingsPopupModal.modal('hide');
		crossSettingsPopupModal.data('open', '1');

		LocationsFunctions.openLocationsModal();
	},

	createAvailableImportSettingsDropdown: function(dropdownID, settingList) {
		'use strict';
		var possibleValues = [];
		$.each(settingList, function(index, setting) {
			possibleValues.push(ImportCrosses.convertSettingToSelect2Item(setting));
		});

		$('#' + getJquerySafeId(dropdownID)).select2({
			initSelection: function(element, callback) {
				$.each(possibleValues, function(index, value) {
					if (value.id === element.val()) {
						callback(value);
					}
				});
			},
			query: function(query) {
				var data = {
					results: possibleValues
				};
				// return the array that matches
				data.results = $.grep(data.results, function(item) {
					return ($.fn.select2.defaults.matcher(query.term,
						item.text));

				});
				query.callback(data);
			}
		});
	},

	convertSettingToSelect2Item: function(setting) {
		'use strict';
		return {
			id: setting.name,
			text: setting.name,
			description: setting.name,
			programPresetId: setting.programPresetId
		};
	},

	retrieveAvailableImportSettings: function() {
		'use strict';
		//TODO Handle errors for ajax request
		return $.ajax({
			url: ImportCrosses.CROSSES_URL + '/retrieveSettings',
			type: 'GET',
			cache: false
		});
	},

	deleteImportSettings: function(programPresetId) {
		'use strict';
		return $.ajax({
			url: ImportCrosses.CROSSES_URL + '/deleteSetting/' + programPresetId,
			type: 'DELETE',
			cache: false,
			global: false
		});
	},

	submitCrossImportSettings: function() {
		'use strict';
		var settingData = ImportCrosses.constructSettingsObjectFromForm();

		if (!ImportCrosses.isCrossImportSettingsValid(settingData)) {
			return;
		}

		var targetURL = ImportCrosses.CROSSES_URL + '/submit';
		var settingsForSaving = false;

		if ($('#presetName').val().trim() !== '') {
			targetURL = ImportCrosses.CROSSES_URL + '/submitAndSaveSetting';
			settingsForSaving = true;
		}

		return $.ajax({
			headers: {
				Accept: 'application/json',
				'Content-Type': 'application/json'
			},
			url: targetURL,
			type: 'POST',
			cache: false,
			data: JSON.stringify(settingData),
			success: function(data) {
				if (data.success === '0') {
					showErrorMessage('', $.fieldbookMessages.errorImportFailed);
				} else {
					$('#crossSettingsModal').modal('hide');

					if (settingsForSaving) {
						// as per UI requirements, we also display a success message regarding the saving of the settings
						// if an error in the settings saving has occurred, program flow would have continued in the data.success === '0' branch
						// hence, we can safely assume that settings have been properly saved at this point
						showSuccessfulMessage('', crossingSettingsSaved);
					}
				}
			},
			error: function() {
				showErrorMessage('', $.fieldbookMessages.errorImportCrossesSettingsFailed);
			}
		});
	},

	isBreedingMethodSelectedValid: function() {
		'use strict';
		var radioValue = $('#selectMethodForAllCrosses').prop('checked');
		var breedingMethodId = $('#breedingMethodDropdown').select2('val');
		if (radioValue && (!breedingMethodId || breedingMethodId === '')) {
			return false;
		} else {
			return true;
		}
	},

	isCrossImportSettingsValid: function(importSettings) {
		'use strict';
		var valid = true;
		if($('#harvestMonthDropdown').val() === '') {
			valid = false;
			showErrorMessage('', $.fieldbookMessages.errorNoHarvestMonth);
		}
		if (!importSettings.additionalDetailsSetting.harvestLocationId) {
			valid = false;
			showErrorMessage('', $.fieldbookMessages.errorNoHarvestLocation);
		}
		if (importSettings.isUseManualSettingsForNaming) {
			if (!importSettings.crossNameSetting.prefix || importSettings.crossNameSetting.prefix === '') {
				valid = false;
				showErrorMessage('', $.fieldbookMessages.errorNoNamePrefix);
			} else if (!importSettings.crossNameSetting.separator || importSettings.crossNameSetting.separator === '') {
				valid = false;
				showErrorMessage('', $.fieldbookMessages.errorNoParentageDesignationSeparator);
			}

			if (!ImportCrosses.validateStartingSequenceNumber(importSettings.crossNameSetting.startNumber)) {
				return false;
			}
		}

		return valid;
	},

	updateDisplayedSequenceNameValue: function() {
		'use strict';
		var value = $('#startingSequenceNumber').val();
		if(ImportCrosses.validateStartingSequenceNumber(value)) {
			ImportCrosses.retrieveNextNameInSequence(ImportCrosses.updateNextSequenceName
				, function() { showErrorMessage('', $.fieldbookMessages.errorNoNextNameInSequence); });
		}
	},

	updateNextSequenceName : function(data) {
		if (data.success === '1') {
			$('#importNextSequenceName').text(data.sequenceValue);
		} else {
			showErrorMessage('', data.error);
		}
	},

	retrieveNextNameInSequence: function(success, fail) {
		'use strict';
		var settingData = ImportCrosses.constructSettingsObjectFromForm();

		$.ajax({
			headers: {
				Accept: 'application/json',
				'Content-Type': 'application/json'
			},
			url: ImportCrosses.CROSSES_URL + '/generateSequenceValue',
			type: 'POST',
			data: JSON.stringify(settingData),
			cache: false
		}).done(function(data) {
			success(data);
		}).fail(function() {
			fail();
		});
	},

	constructSettingsObjectFromForm: function() {
		'use strict';
		var settingObject = {};
		settingObject.name = $('#presetName').val();

		settingObject.breedingMethodSetting = {};
		settingObject.breedingMethodSetting.methodId = $('#breedingMethodDropdown').select2('val');

		settingObject.breedingMethodSetting.basedOnStatusOfParentalLines = $('#selectUseParentalStatus').prop('checked');
		settingObject.breedingMethodSetting.basedOnImportFile = $('#selectMethodInImportFile').prop('checked');

		settingObject.crossNameSetting = {};
		settingObject.crossNameSetting.prefix = $('#crossPrefix').val();
		settingObject.crossNameSetting.suffix = $('#crossSuffix').val();
		settingObject.crossNameSetting.addSpaceBetweenPrefixAndCode = $('input:radio[name=hasPrefixSpace]:checked').val() === 'true';
		settingObject.crossNameSetting.addSpaceBetweenSuffixAndCode = $('input:radio[name=hasSuffixSpace]:checked').val() === 'true';
		settingObject.crossNameSetting.numOfDigits = $('#sequenceNumberDigits').val();
		settingObject.crossNameSetting.separator = $('#parentageDesignationSeparator').val();
		settingObject.crossNameSetting.startNumber = $('#startingSequenceNumber').val();
		settingObject.crossNameSetting.saveParentageDesignationAsAString =
			$('input:radio[name=hasParentageDesignationName]:checked').val() === 'true';
		settingObject.preservePlotDuplicates =  ImportCrosses.preservePlotDuplicates;
		settingObject.applyNewGroupToPreviousCrosses = !$('#applyGroupingCheckBox').prop('checked');
		settingObject.isUseManualSettingsForNaming = $('input:radio[name=manualNamingSettings]:checked').val() === 'true';
		settingObject.additionalDetailsSetting = {};

		var locationSelected = $('#locationDropdown').select2('data');
		if (locationSelected && locationSelected.id) {
			settingObject.additionalDetailsSetting.harvestLocationId = locationSelected.id;
		}
		
		if ($('#harvestYearDropdown').val() !== '' && $('#harvestMonthDropdown').val() !== '') {
			settingObject.additionalDetailsSetting.harvestDate = $('#harvestYearDropdown').val() + '-' + $('#harvestMonthDropdown').val() + '-01';
		}

		return settingObject;
	},

	populateHarvestMonthDropdown: function(dropdownID) {
		'use strict';
		ImportCrosses.retrieveHarvestMonths().done(function(monthData) {
			var dropdownSelect = $('#' + dropdownID);
			dropdownSelect.empty();
			dropdownSelect.select2({
				placeholder: 'Month',
				allowClear: true,
				data: monthData,
				minimumResultsForSearch: -1
			});
		});
	},

	populateHarvestYearDropdown: function(dropdownID) {
		'use strict';
		ImportCrosses.retrieveHarvestYears().done(function(yearData) {
			var dropdownData = [];
			var dropdownSelect = $('#' + dropdownID);
			dropdownSelect.empty();
			$.each(yearData, function(index, value) {
				dropdownData.push({
					id: value,
					text: value
				});
			});

			dropdownSelect.select2({
				minimumResultsForSearch: -1,
				data: dropdownData
			});

			//select the current year; the current year is the middle with the options as -10, current, +10 years
			var currentYearIndex = parseInt(yearData.length/2);
			dropdownSelect.select2('val', yearData[currentYearIndex]);
		});
	},

	retrieveHarvestMonths: function() {
		'use strict';
		//TODO handle errors for ajax request
		return $.ajax({
			url: ImportCrosses.CROSSES_URL + '/getHarvestMonths',
			type: 'GET',
			cache: false
		});
	},

	retrieveHarvestYears: function() {
		'use strict';
		//TODO handle errors for ajax request
		return $.ajax({
			url: ImportCrosses.CROSSES_URL + '/getHarvestYears',
			type: 'GET',
			cache: false
		});
	},

	exportCrosses: function() {
		'use strict';
		//TODO handle errors for ajax request
		return $.ajax({
			url: ImportCrosses.CROSSES_URL + '/export',
			type: 'GET',
			cache: false
		});
	},

	downloadCrosses: function() {
		'use strict';
		ImportCrosses.exportCrosses().done(function(result) {
			if (result.isSuccess) {
				var downloadUrl = ImportCrosses.CROSSES_URL + '/download/file';

				$.fileDownload(downloadUrl, {
					httpMethod: 'POST',
					data: result
				});
			} else {
				createErrorNotification(crossingExportErrorHeader, result.errorMessage);
			}
		});
	},
	openSaveListModal: function() {
		'use strict';
		var  germplasmTreeNode = $('#germplasmFolderTree').dynatree('getTree');
		//TODO handle errors for ajax request
		$.ajax({
			url: '/Fieldbook/ListTreeManager/saveCrossesList/',
			type: 'GET',
			cache: false,
			success: function(html) {
				$('#saveGermplasmRightSection').html(html);
				$('#saveListTreeModal').modal({
					show: true,
					keyboard: true,
					backdrop: 'static'
				});
				$('#saveListTreeModal').data('is-save-crosses', '1');

				TreePersist.preLoadGermplasmTreeState(false, '#germplasmFolderTree', true);

				//we preselect the program lists
				if (germplasmTreeNode !== null && germplasmTreeNode.getNodeByKey('LOCAL') !== null) {
					germplasmTreeNode.getNodeByKey('LOCAL').activate();
				}
			},
			error: function() {
				//TODO process errors
			}
		});
	},

	openGermplasmModalFromExistingCrossesView : function (gid, desig) {
		'use strict';
		$('#openGermplasmModal').off('hidden.bs.modal');
		$('#existingCrossesModal').off('hidden.bs.modal');
		$('#existingCrossesModal').off('shown.bs.modal');
		$('#existingCrossesModal').modal('hide');
		openGermplasmDetailsPopopWithGidAndDesig(gid, desig);
		$('#openGermplasmModal').one('hidden.bs.modal', function () {
			$('#openGermplasmModal').modal('hide');
			$('#existingCrossesModal').on('hidden.bs.modal', function () {
				$('#existingCrossesModal').modal ('hide');
				$('#openCrossesListModal').one('shown.bs.modal', function() {
					// After the modal window is shown, make sure that the table header is properly adjusted.
					$('#preview-crosses-table').resize();
				}).modal({ backdrop: 'static', keyboard: true });
			});
			$('#existingCrossesModal').one('shown.bs.modal', function() {
				// After the modal window is shown, make sure that the table header is properly adjusted.
				$('#existing-crosses-table').resize();
			}).modal({ backdrop: 'static', keyboard: true });
		});
	},

	openGermplasmModal : function (gid, desig) {
		'use strict';
		$('#openGermplasmModal').off('hidden.bs.modal');
		$('#openCrossesListModal').off('hidden.bs.modal');
		$('#openCrossesListModal').off('shown.bs.modal');
		$('#openCrossesListModal').modal('hide');
		openGermplasmDetailsPopopWithGidAndDesig(gid, desig);
		$('#openGermplasmModal').one('hidden.bs.modal', function () {
			$('#openGermplasmModal').modal ('hide');
			$('#openCrossesListModal').one('shown.bs.modal', function() {
				// After the modal window is shown, make sure that the table header is properly adjusted.
				$('#preview-crosses-table').resize();
			}).modal({ backdrop: 'static', keyboard: true });
		});
	}
};

$(document).ready(function() {
	'use strict';
	$('.import-crosses').off('click');
	$('.btn-import-crosses').off('click');
	$('.import-crosses').on('click', ImportCrosses.showPopup);
	$('.btn-import-crosses').on('click', ImportCrosses.doSubmitImport);
	$('.import-crosses-section .modal').on('hide.bs.modal', function() {
		$('div.import-crosses-file-upload').parent().parent().removeClass('has-error');
	});

	// handler for the manualNamingSettings display
	$('input:radio[name=manualNamingSettings]').on('change', function() {
		if ($('input:radio[name=manualNamingSettings]:checked').val() === 'true') {
			$('#manualNamingSettingsPanel').removeClass('fbk-hide');
		} else {
			$('#manualNamingSettingsPanel').addClass('fbk-hide');
		}
	});

	// handler for the showing on records with Alerts filtering
	$('#showOnlyRecordsWithAlerts').on('change', function() {
		if ( $('#showOnlyRecordsWithAlerts').is(':checked')) {
			$('#preview-crosses-table').DataTable().column(0).search('View Existing Crosses', true, false).draw();
		} else {
			$('#preview-crosses-table').DataTable().column(0).search('', true, false).draw();
		}
	});

	$('#checkExistingCrosses').on('change', function() {
		if ( $('#checkExistingCrosses').is(':checked')) {
			$('#showOnlyRecordsWithAlertsDiv').removeClass('fbk-hide');
		} else {
			$('#showOnlyRecordsWithAlertsDiv').addClass('fbk-hide');
		}
	});
});
