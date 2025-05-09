/*globals displayGermplasmListTree, changeBrowseGermplasmButtonBehavior, saveGermplasmReviewError */
/*globals $,showErrorMessage, showInvalidInputMessage, getDisplayedTreeName,ImportCrosses,listShouldNotBeEmptyError,getJquerySafeId,
validateAllDates, saveListSuccessfullyMessage */
/*globals listParentFolderRequired, listNameRequired */
/*globals listDateRequired, listTypeRequired, moveToTopScreen */
/*globals TreePersist, showSuccessfulMessage, console, germplasmEntrySelectError */
/*exported saveGermplasmList, openSaveListModal*/

var SaveAdvanceList = {};

(function() {
	'use strict';
	SaveAdvanceList.initializeGermplasmListTree = function() {
		displayGermplasmListTree('germplasmFolderTree', true, 1);
		$('#germplasmFolderTree').off('bms.tree.node.activate').on('bms.tree.node.activate', function () {
			var id = $('#germplasmFolderTree').dynatree('getTree').getActiveNode().data.key;
			if (id == 'CROPLISTS') {
				ListTreeOperation.hideFolderDiv('#addGermplasmFolderDiv');
				ListTreeOperation.hideFolderDiv('#renameGermplasmFolderDiv');
			}
		});
		changeBrowseGermplasmButtonBehavior(false);
		$('#saveListTreeModal').off('hide.bs.modal');
		$('#saveListTreeModal').on('hide.bs.modal', function() {
			TreePersist.saveGermplasmTreeState(false, '#germplasmFolderTree');
		});
		$('#saveListTreeModal').on('hidden.bs.modal', function() {
			$('#germplasmFolderTree').dynatree('getTree').reload();
			changeBrowseGermplasmButtonBehavior(false);
		});
	};

	SaveAdvanceList.openSaveListModal = function(object) {
		if (parseInt($('#reviewAdvanceStudyModal .total-review-items').html(), 10) < 1) {
			showErrorMessage('', saveGermplasmReviewError);
			return false;
		}
		$('#reviewAdvanceStudyModal').modal('hide');

		var listIdentifier = $(object).attr('id'),
		germplasmTreeNode = $('#germplasmFolderTree').dynatree('getTree'),
		additionalLazyLoadUrl = '/1';
		$.ajax(
			{
				url: '/Fieldbook/ListTreeManager/saveList/' + listIdentifier,
				type: 'GET',
				cache: false,
				success: function(html) {
					$('#saveListTreeModal').data('is-save-crosses', '0');
					$('#saveGermplasmRightSection').html(html);
					setTimeout(function() {
						$('#saveListTreeModal').modal({ backdrop: 'static', keyboard: true });
						TreePersist.preLoadGermplasmTreeState(false, '#germplasmFolderTree', true);
					}, 300);
					//we preselect the program lists
					if (germplasmTreeNode !== null && germplasmTreeNode.getNodeByKey('LISTS') !== null) {
						germplasmTreeNode.getNodeByKey('LISTS').activate();
					}
				}
			}
		);
	};

	SaveAdvanceList.doAdvanceStudy = function() {

		var serializedData;

		$('input[type=checkbox][name=methodChoice]').prop('disabled', false);
		serializedData = $('#advanceStudyModalForm').serialize();

		$.ajax({
			url: '/Fieldbook/StudyManager/advance/study',
			type: 'POST',
			data: serializedData,
			cache: false,
			success: function(data) {
				var advanceGermplasmChangeDetail = [];
				if (data.isSuccess === '0') {
					showErrorMessage('page-advance-modal-message', data.message);
				} else {
					if (data.listSize === 0) {
						showErrorMessage('page-advance-modal-message', listShouldNotBeEmptyError);
					} else {
						advanceGermplasmChangeDetail = (data.advanceGermplasmChangeDetails);
						$('#advanceStudyModal').modal('hide');
						if (advanceGermplasmChangeDetail.length === 0) {
							SaveAdvanceList.reviewAdvanceList(data.uniqueId);
						} else {
							showAdvanceGermplasmChangeConfirmationPopup(advanceGermplasmChangeDetail, data.uniqueId);
						}
					}
				}

			},
			error: function(jqXHR, textStatus, errorThrown) {
				console.log('The following error occured: ' + textStatus, errorThrown);
			}
		});
	};

	SaveAdvanceList.reviewAdvanceList = function(uniqueId) {
		$.ajax({
			url: '/Fieldbook/StudyManager/advance/study/info?uniqueId=' + uniqueId,
			type: 'GET',
			cache: false,
			success: function(html) {
				$('#advanceStudyModal').modal('hide');

				$('#review-advance-study-modal-div').html(html);
				$('.btn-cancel-review').off('click');
				$('.btn-cancel-review').on('click', function() {
						$('#reviewAdvanceStudyModal').modal('hide');
						setTimeout(function() {$('#advanceStudyModal').modal({ backdrop: 'static', keyboard: true });}, 300);
					});
				setTimeout(function() {
					$('#reviewAdvanceStudyModal').off('shown.bs.modal');
					$('#reviewAdvanceStudyModal').on('shown.bs.modal', function() {
							SaveAdvanceList.setupAdvanceListForReview();
						});
					$('#reviewAdvanceStudyModal').modal({ backdrop: 'static', keyboard: true });
				}, 300);

			},
			error: function(jqXHR, textStatus, errorThrown) {
				console.log('The following error occured: ' + textStatus, errorThrown);
			}
		});
	};
	SaveAdvanceList.verifyCheckboxesForSelectAll = function() {
        'use strict';
		if($('.review-select-all:checked') && $('input.reviewAdvancingListGid:not(:checked)').length > 0) {
			//this is the time we check if there are actual uncheck bxoes so we can uncheck this one
			$('.review-select-all').prop('checked', false);

		}
	};

    // Select / Unselect checked entries while moving one page to another
    SaveAdvanceList.setSelectedEntries = function () {
        $('[type="checkbox"]:not(:checked)', $('.advance-study-list-table .advance-germplasm-items').DataTable().rows().nodes()).parent().parent().removeClass('selected');
        $('[type="checkbox"]:checked', $('.advance-study-list-table .advance-germplasm-items').DataTable().rows().nodes()).parent().parent().addClass('selected');
    };

	SaveAdvanceList.setupAdvanceListForReview = function() {
		var sectionContainerDiv = 'reviewAdvanceStudyModal';
		
		$('#'+getJquerySafeId(sectionContainerDiv) + ' .review-select-all').on('change', function(event){
			//select all the checkbox in the section container div										
            //needed set time out since chrme is not able to rnder properly the checkbox if its checked or not
            setTimeout(function(){

                var rows = $(".advance-study-list-table .advance-germplasm-items").DataTable().rows().nodes();

                var isChecked = $('#'+getJquerySafeId(sectionContainerDiv) + ' .review-select-all').prop('checked');

                if(isChecked){
                    $('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').addClass('selected');
                    $('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').addClass('manual-selected');
                    $('input[type="checkbox"]', rows).prop('checked', 'checked').parent('td').parent('tr').addClass('selected').addClass('manual-selected');
                } else {
                    $('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').removeClass('selected');
                    $('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').removeClass('manual-selected');
                    $('#'+getJquerySafeId(sectionContainerDiv) + ' input.reviewAdvancingListGid').prop('checked', isChecked);
                    $('input[type="checkbox"]', rows).prop('checked', isChecked).parent('td').parent('tr').removeClass('selected').removeClass('manual-selected');
                }

                // Display total number of selected entries
                var selectedRows = $('[type="checkbox"]:checked', $('.advance-study-list-table .advance-germplasm-items').DataTable().rows().nodes()).length;
                $('#' + getJquerySafeId(sectionContainerDiv) + ' .numberOfAdvanceSelected').html(selectedRows);
            },10);
        });
		
		$('#' + sectionContainerDiv + ' .advance-study-list-table').tableSelect({
			onClick: function(row) {
				//we clear all check
				if ($('#' + sectionContainerDiv + ' .advance-study-list-table').data('check-click') === '1') {
					$('#' + sectionContainerDiv + ' .advance-study-list-table').data('check-click', '0');
					$('#' + sectionContainerDiv + ' .advance-study-list-table tr.manual-selected input.reviewAdvancingListGid:checked').parent().parent().addClass('selected');
					$('#' + sectionContainerDiv + ' .advance-study-list-table tr:not(.manual-selected) input.reviewAdvancingListGid:checked').parent().parent().addClass('selected');
					$('#' + sectionContainerDiv + ' .advance-study-list-table tr.selected input.reviewAdvancingListGid:not(:checked)').parent().parent().removeClass('selected');
				} else {
					var rows = $(".advance-study-list-table .advance-germplasm-items").DataTable().rows().nodes();
					$('input[type="checkbox"]', rows).prop('checked', false).parent('td').parent('tr').removeClass('manual-selected');
					if ($(row).hasClass('selected')) {
						$(row).find('input.reviewAdvancingListGid').prop('checked', true);
					} else {
						$(row).find('input.reviewAdvancingListGid').prop('checked', false);
					}
				}

                // Display total number of selected entries
                var selectedRows = $('[type="checkbox"]:checked', $('.advance-study-list-table .advance-germplasm-items').DataTable().rows().nodes()).length;
                $('#' + getJquerySafeId(sectionContainerDiv) + ' .numberOfAdvanceSelected').html(selectedRows);
				SaveAdvanceList.verifyCheckboxesForSelectAll();
			},
			onCtrl: function(row) {

				$('#' + sectionContainerDiv + ' .advance-study-list-table tr.selected input.reviewAdvancingListGid').prop('checked', true);
				$('#' + sectionContainerDiv + ' .advance-study-list-table tr:not(.selected) input.reviewAdvancingListGid').prop('checked', false);
				if ($(row).hasClass('manual-selected') || $(row).hasClass('selected')) {
					$(row).find('input.reviewAdvancingListGid').prop('checked', true);
					$(row).addClass('selected');
				}else {
					$(row).find('input.reviewAdvancingListGid').prop('checked', false);
					$(row).removeClass('selected');
				}
				if ($('#' + sectionContainerDiv + ' .advance-study-list-table').data('check-click') === '1') {
					$('#' + sectionContainerDiv + ' .advance-study-list-table').data('check-click', '0');
					if ($(row).hasClass('selected') && $(row).hasClass('manual-selected') === false) {
						$(row).find('input.reviewAdvancingListGid').prop('checked', false);
						$(row).removeClass('selected');
					}
				}
                // Display total number of selected entries
                var selectedRows = $('[type="checkbox"]:checked', $('.advance-study-list-table .advance-germplasm-items').DataTable().rows().nodes()).length;
                $('#' + getJquerySafeId(sectionContainerDiv) + ' .numberOfAdvanceSelected').html(selectedRows);
				SaveAdvanceList.verifyCheckboxesForSelectAll();
			},
			onShift: function() {
				var selectedRows = $(".advance-study-list-table .advance-germplasm-items").DataTable().rows(['.selected']).nodes();
				$('input[type="checkbox"]', selectedRows).prop('checked', true).parent('td').parent('tr').addClass('selected').addClass('manual-selected');
				var unselectedRows = $(".advance-study-list-table .advance-germplasm-items").DataTable().rows([':not(.selected)']).nodes();
				$('input[type="checkbox"]', unselectedRows).prop('checked', false).removeClass('manual-selected');
				
                // Display total number of selected entries
                var selectedRows = $('[type="checkbox"]:checked', $('.advance-study-list-table .advance-germplasm-items').DataTable().rows().nodes()).length;
                $('#' + getJquerySafeId(sectionContainerDiv) + ' .numberOfAdvanceSelected').html(selectedRows);
				SaveAdvanceList.verifyCheckboxesForSelectAll();
			}
		});

		$('#' + sectionContainerDiv + ' .advance-study-list-table input.reviewAdvancingListGid').on('click', function() {
			$('#' + sectionContainerDiv + ' .advance-study-list-table').data('check-click', '1');
			if ($(this).is(':checked')) {
				//we highlight
				$(this).parent().parent().addClass('selected');
				$(this).parent().parent().addClass('manual-selected');
			} else {
				$(this).parent().parent().removeClass('selected');
				$(this).parent().parent().removeClass('manual-selected');
			}
			$('#' + sectionContainerDiv + ' .advance-study-list-table tr.manual-selected input.reviewAdvancingListGid').prop('checked', true);
			$('#' + sectionContainerDiv + ' .advance-study-list-table tr.manual-selected').addClass('selected');
			$('#' + sectionContainerDiv + ' .advance-study-list-table tr:not(.manual-selected)').remove('selected');
			SaveAdvanceList.verifyCheckboxesForSelectAll();
		});

		$('#reviewAdvanceStudyModal').off('shown.bs.modal');
		$('#reviewAdvanceStudyModal .delete-entries').off('click');
		$('#reviewAdvanceStudyModal .select-all-entries').off('click');
		$('#reviewAdvanceStudyModal .delete-entries').on('click', SaveAdvanceList.deleteSelectedEntries);
		$('#reviewAdvanceStudyModal .select-all-entries').on('click', SaveAdvanceList.selectAllReviewEntries);

		$.contextMenu({
			selector: '#' + sectionContainerDiv + ' .advance-germplasm-items',
			items: {
				deleteSelected: {
					name: "Delete Selected Entries", callback: function() {
						SaveAdvanceList.deleteSelectedEntries();
					}
				},
				selectAll: {
					name: "Select All", callback: function() {
						SaveAdvanceList.selectAllReviewEntries();
					}
				}
			}
		});

		new BMS.Fieldbook.AdvancedGermplasmListDataTable('#' + sectionContainerDiv + ' .advance-germplasm-items', '#' + sectionContainerDiv);
		$('#advance-study-germplasm-list').css('opacity', '1');
		if ($('.total-review-items').html() === '0') {
			$('.review-select-all-section').hide();
		} else {
			$('.review-select-all-section').show();
		}
	};

	SaveAdvanceList.selectAllReviewEntries = function() {
		var sectionContainerDiv = 'reviewAdvanceStudyModal';
		var isChecked = true;
		$('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').removeClass('selected');
		$('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').removeClass('manual-selected');
		$('#'+getJquerySafeId(sectionContainerDiv) + ' input.reviewAdvancingListGid').prop('checked', isChecked);

        var rows = $(".advance-study-list-table .advance-germplasm-items").DataTable().rows().nodes();

		if(isChecked) {
			$('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').addClass('selected');
			$('#'+getJquerySafeId(sectionContainerDiv) + ' .advance-study-list-table tr').addClass('manual-selected');
            $('input[type="checkbox"]', rows).prop('checked', 'checked').parent('td').parent('tr').addClass('selected').addClass('manual-selected');
		}
		$('#' + getJquerySafeId(sectionContainerDiv) + ' .numberOfAdvanceSelected').html($('#' + getJquerySafeId(sectionContainerDiv) +
			' tr.primaryRow.selected').length);
		$('#' + getJquerySafeId(sectionContainerDiv) + ' .review-select-all').prop('checked', isChecked);
	};

	SaveAdvanceList.deleteSelectedEntries = function() {
		var entryNums = '',
			uniqueId = $('.btn-save-advance-list').attr('id');
		$('[type="checkbox"]:checked', $('.advance-study-list-table .advance-germplasm-items').DataTable().rows().nodes()).each(function() {
			if (entryNums !== '') {
				entryNums += ',';
			}
			entryNums += $(this).data('entry');
		});
		if (entryNums.length === 0) {
			showErrorMessage('page-message', germplasmEntrySelectError);
			moveToTopScreen();
			return;
		}

		$.ajax({
			url: '/Fieldbook/StudyManager/advance/study/delete/entries',
			type: 'POST',
			data: {
				entryNums: entryNums,
				uniqueId: uniqueId
			},
			cache: false,
			success: function(html) {
				$('#review-advance-study-modal-div .review-advance-records').html($(html).find('.review-advance-records').html());
				$('.btn-cancel-review').off('click');
				$('.btn-cancel-review').on('click', function() {
						$('#reviewAdvanceStudyModal').modal('hide');
						setTimeout(function() { $('#advanceStudyModal').modal({ backdrop: 'static', keyboard: true });}, 300);
					});

				setTimeout(function() { SaveAdvanceList.setupAdvanceListForReview();}, 300);

			},
			error: function(jqXHR, textStatus, errorThrown) {
				console.log('The following error occured: ' + textStatus, errorThrown);
			}
		});
	};
})();

