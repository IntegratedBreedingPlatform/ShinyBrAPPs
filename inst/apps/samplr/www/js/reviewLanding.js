function reviewLandingSetup() {
	'use strict';
	var hasMeasurements = $('#study' + getCurrentStudyIdInTab() + ' #has-measurements').val() === 'true',
	hasFieldmap = false;
	$('#study' + getCurrentStudyIdInTab() + ' ' + '.toggle-icon').click(function() {
		var expandedImg = $(this).find('.section-expanded');
		var collapsedImg = $(this).find('.section-collapsed');
		expandedImg.toggle();
		collapsedImg.toggle();
	});
	$('#study' + getCurrentStudyIdInTab() + ' ' + '.section-toggle').on('click', function() {
		var className = $(this).data('target');
		$('#study' + getCurrentStudyIdInTab() + ' ' + className).slideToggle();
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #open-study-url-link').click(function() {
		selectedTableIds[0] = $('#div-study-tab-' + getCurrentStudyIdInTab() + ' #studyId').val();
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #advance-study-url-link').click(function() {
		selectedTableIds[0] = $('#div-study-tab-' + getCurrentStudyIdInTab() + ' #studyId').val();
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #div-study-tab-' + getCurrentStudyIdInTab() + ' #fieldmap-url-link').click(function() {
		selectedTableIds[0] = $('#div-study-tab-' + getCurrentStudyIdInTab() + ' #studyId').val();
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #label-printing-url-link').click(function() {
		selectedTableIds[0] = $('#div-study-tab-' + getCurrentStudyIdInTab() + ' #studyId').val();
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #view-fieldmap-url-link').click(function() {
		selectedTableIds[0] = $('#div-study-tab-' + getCurrentStudyIdInTab() + ' #studyId').val();
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #toggle-additional-link').click(function() {
		if ($(this).text() == showText) {
			$(this).text(hideText);
		} else {
			$(this).text(showText);
		}
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' .dataset-toggle').click(function() {
		loadDatasetDropdown($(this).parent().find('#dataset-selection'));
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #dataset-selection').change(function() {
		if ($(this).val()) {
			var id = $(this).val();
			var name = $(this).find('option:selected').text();
			var selectedId = $(this).find('option:selected').val();
			var found = false;
			if ($(this).val() == 'Please Choose') {
				return;
			}

			$('#study' + getCurrentStudyIdInTab() + ' #measurement-tabs').children().hide();
			$('#study' + getCurrentStudyIdInTab() + ' #measurement-tab-headers').find('li').each(function(index) {
				$(this).removeClass('active');

				if ($(this).prop('id').replace('dataset-li', '') === selectedId) {

					$(this).addClass('active');
					$('#dset-tab-' + id).show();
					found = true;
				}
			});

			if (!found) {
				loadDatasetMeasurementRowsViewOnly(id, name);
			}
		}
	});

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' .nurseryDetails').find('.control-label').each(function(index) {
		if ($(this).text() == 'Field Map Created') {
			if ($(this).parent().parent().find('.div-select-val').text() == 'Yes') {
				hasFieldmap = true;
			}
		}
	});

	if (!hasMeasurements) {
		$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #advance-study-url-link').hide();
		$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #fieldmap-url-link').hide();
		$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #label-printing-url-link').hide();
		$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #export-study-link').hide();
	}

	if (hasFieldmap) {
		$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #fieldmap-url-link').hide();
	}
	truncateStudyVariableNames('.fbk-variable', 10);

	$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-treatment-count').text($('#div-study-tab-' + getCurrentStudyIdInTab() + ' .review-stocks-count').text());

	if ($('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-block-size') && $('#div-study-tab-' + getCurrentStudyIdInTab() + ' .review-stocks-count')) {
		var blockSize = parseInt($('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-block-size').text(), 10);
		var entrySize = parseInt($('#div-study-tab-' + getCurrentStudyIdInTab() + ' .review-stocks-count').text(), 10);
		var totalPlotSize = parseInt($('#div-study-tab-' + getCurrentStudyIdInTab() + ' .review-plots-count').text(), 10);
		var numberOfEnvironments = parseInt($('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-study-number-of-environments').text(), 10);
		var plotSize = 0;
		if (numberOfEnvironments > 0) {
			plotSize = totalPlotSize / numberOfEnvironments;
			$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-number-of-plots').text(plotSize);
			if (blockSize > 0) {
				var blockCount = entrySize / blockSize;
				$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-number-of-blocks').text(blockCount);
				var entriesPerBlock = plotSize / blockSize;
				$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-entries-per-blocks').text(entriesPerBlock);
			} else {
				$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #row-review-number-of-blocks').hide();
			}
		}

		var checkEntriesCount  = parseInt($('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-number-of-check-entries').text(), 10);
		var testEntriesCount = entrySize - checkEntriesCount;
		$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #review-number-of-test-entries').text(testEntriesCount);

	} else {
		$('#div-study-tab-' + getCurrentStudyIdInTab() + ' #row-review-number-of-blocks').hide();
	}
}
