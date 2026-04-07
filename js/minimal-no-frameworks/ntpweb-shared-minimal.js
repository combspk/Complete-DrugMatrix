var NTP = {
    printpage: '#printthispage',
    raAgencies: '#raAgencies',
    actualHostname: window.location.hostname.toLowerCase(),
    productionHostnames: ['ntp', 'www.ntp', 'ntp.niehs.nih.gov', 'www.ntp.niehs.nih.gov', 'tools.niehs.nih.gov', 'ntpsearch', 'ntpsearch.niehs.nih.gov', 'edit', 'rstudio.niehs.nih.gov'],
    devHostNames: ['ntpdev', 'ntpdev.niehs.nih.gov', 'apps2dev.niehs.nih.gov', 'tools2dev.niehs.nih.gov', 'ntpsearch-dev', 'ntpsearch-dev.niehs.nih.gov', 'edit-dev'],
    testHostNames: ['ntptest', 'ntptest.niehs.nih.gov', 'apps2tst.niehs.nih.gov', 'tools2tst.niehs.nih.gov', 'ntpsearch-test', 'ntpsearch-test.niehs.nih.gov', 'edit-tst'],
    environment: 'Prod',
    PublicSiteHostName: 'https://ntp.niehs.nih.gov',
    isProduction: false
}

// check to see if we're on production
for (var i = 0; i < NTP.devHostNames.length; i++) {
    if (NTP.actualHostname === NTP.devHostNames[i]) {
        NTP.environment = 'Dev'
        NTP.PublicSiteHostName = 'https://ntpdev.niehs.nih.gov'
        break;
    }
}
for (var i = 0; i < NTP.testHostNames.length; i++) {
    if (NTP.actualHostname === NTP.testHostNames[i]) {
        NTP.environment = 'Test'
        NTP.PublicSiteHostName = 'https://ntptest.niehs.nih.gov'
        break;
    }
}
for (var i = 0; i < NTP.productionHostnames.length; i++) {
    if (NTP.actualHostname === NTP.productionHostnames[i]) {
        NTP.environment = 'Prod'
        NTP.PublicSiteHostName = 'https://ntp.niehs.nih.gov'
        NTP.isProduction = true
        break;
    }
}

// prevent cross-frame scripting by forcing the document to be the top - only for production instance
if (NTP.actualHostname != "rstudio.niehs.nih.gov"){//make exception for Posit connect to avoid redirection
    if (top != self) top.location = self.location;
}

$(function () {
    // register print this page event.
    $(NTP.printpage).attr('href', '#').click(function () { window.print(); });

    //register regulatory actions event.
    $(NTP.raAgencies).change(function () { location.href = "#" + $(this).val(); });
});

/*  this removes placeholder text before printing - Added by Mark Colebank 5/8/2015 
called in body tag on forms <body onbeforeprint="removePlaceholder()"> */
function removePlaceholder() {
    $("input").removeAttr("placeholder");
    $("textarea").removeAttr("placeholder");
}

//adds IE support for includes() JS method - see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/includes
if (!String.prototype.includes) {
    String.prototype.includes = function() {
        'use strict';
        return String.prototype.indexOf.apply(this, arguments) !== -1;
    };
}

//define external link disclaimer title attribute; used within ntp.footer.js and ntp.external-links.js
var ntpExternalDisclaimer = 'This link is to a non-NTP website. Links do not constitute endorsement by NTP of the linked website. Visitors to the linked website will be subject to the website privacy policies. These practices may be different than those of this NTP website.';
// ===============================================================
// Footer (requires jQuery, ntp.common.js, & Foundation MediaQuery)
// ===============================================================
$(document).ready(function(){

	let footerHeadings = $('.link-category h5, .link-category .h5');

	function footerMobile() {
		$('.link-category ul').hide();
		footerHeadings.unbind('click keyup').attr('tabindex', '0').on({
			click: function() {
				$(this).next('ul').slideToggle();
				$(this).toggleClass('footer-nav-active');
			},
			keyup: function(event) {
				if(event.keyCode == 13){
					$(this).click();
				}
			}
		});
	}
	function footerDesktop() {
		footerHeadings.removeAttr('tabindex').off('click keyup').removeClass('footer-nav-active');
		$('.link-category ul').show();
	}

	//trigger at breakpoint
	onFooterMobile = function() {
		if (Foundation.MediaQuery.is('small only')) {
			footerMobile();
		} else {
			footerDesktop();
		}
	}

	//on page load and media query change, initialize onFooterMobile();
	$(window).on('load changed.zf.mediaquery', onFooterMobile);

	//add title attribute to external links in the footer
	$('#footer a.external, .footer-logos a').attr('title', ntpExternalDisclaimer);

});
"use strict";
/*jslint browser: true*/
/*global  $*/
// Javascript for Thunderstone search
$(function() {
	// default id to query
	var ids = $('[id^=query]').filter(".queryAutocomplete");	
    // add autocomplete
	for(var i =0;i<ids.length ;i++)
	{
		var id =$('[id^=query]').filter(".queryAutocomplete")[i].id;
		autoComplete(id);
	}
	function autoComplete(id)
	{
		$("#" + id + ".queryAutocomplete").autocomplete({
			source: function(request, response) {
				var action = $("#"+id+"").closest("form").attr("action");
				var url = "//seek.niehs.nih.gov/texis/autocomplete.json";
				var profile = "ntp";
				$.ajax({
					dataType: "jsonp",
					url: url,
					data: {
						pr: profile,
						term: request.term
					},
					success: function(data) {
						response(data.completions);
					}
				});
			},
			minLength: 2,
			//open: function(e, ui) {
				// drill into the menu and wrap the term and the completions
				// in spans for styling.  Hopefully JQueryUI will start doing this
				// automatically in the future.
//				var term = $("#"+id+"").val();
//				var acData = $(this).data("uiAutocomplete");
//				acData.menu.element.find("a").each(function() {
//					var a = $(this);
//					var completion = a.text();
//					var pos = completion.indexOf(term);
//					var txt = "";
//					if(pos !== 0) {
//						txt += "<span class='ui-autocomplete-completion'>" + completion.substr(0, pos) + "</span>";
//					}
//					txt += "<span class='ui-autocomplete-term'>" + term + "</span>";
//					if(pos + term.length < completion.length) {
//						txt += "<span class='ui-autocomplete-completion'>" + completion.substr(pos + term.length) + "</span>";
//					}
//					a.html(txt);
//				});
//			},
			select: function(event, ui) {
				$("#"+id+"").val(ui.item.value).closest("form").submit();
			}
		});
	}
});

// ===============================================================
// U.S. Government header accordion toggle
// ===============================================================
$(document).ready(function() {
    var e = $(".usa-closed");
    $(".usa-accordion-button").on("click", function(t) {
        $(e).hasClass("usa-closed") ? ($(this).attr("aria-expanded", "true"), $(e).removeClass("usa-closed"), $(e).addClass("usa-open")) : ($(this).attr("aria-expanded", "false"), $(e).addClass("usa-closed"), $(e).removeClass("usa-open"))
    })
});
//# sourceMappingURL=data:application/json;charset=utf8;base64,eyJ2ZXJzaW9uIjozLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiIiwic291cmNlcyI6WyJudHB3ZWItc2hhcmVkLW1pbmltYWwuanMiXSwic291cmNlc0NvbnRlbnQiOlsidmFyIE5UUCA9IHtcbiAgICBwcmludHBhZ2U6ICcjcHJpbnR0aGlzcGFnZScsXG4gICAgcmFBZ2VuY2llczogJyNyYUFnZW5jaWVzJyxcbiAgICBhY3R1YWxIb3N0bmFtZTogd2luZG93LmxvY2F0aW9uLmhvc3RuYW1lLnRvTG93ZXJDYXNlKCksXG4gICAgcHJvZHVjdGlvbkhvc3RuYW1lczogWydudHAnLCAnd3d3Lm50cCcsICdudHAubmllaHMubmloLmdvdicsICd3d3cubnRwLm5pZWhzLm5paC5nb3YnLCAndG9vbHMubmllaHMubmloLmdvdicsICdudHBzZWFyY2gnLCAnbnRwc2VhcmNoLm5pZWhzLm5paC5nb3YnLCAnZWRpdCddLFxuICAgIGRldkhvc3ROYW1lczogWydudHBkZXYnLCAnbnRwZGV2Lm5pZWhzLm5paC5nb3YnLCAnYXBwczJkZXYubmllaHMubmloLmdvdicsICd0b29sczJkZXYubmllaHMubmloLmdvdicsICdudHBzZWFyY2gtZGV2JywgJ250cHNlYXJjaC1kZXYubmllaHMubmloLmdvdicsICdlZGl0LWRldiddLFxuICAgIHRlc3RIb3N0TmFtZXM6IFsnbnRwdGVzdCcsICdudHB0ZXN0Lm5pZWhzLm5paC5nb3YnLCAnYXBwczJ0c3QubmllaHMubmloLmdvdicsICd0b29sczJ0c3QubmllaHMubmloLmdvdicsICdudHBzZWFyY2gtdGVzdCcsICdudHBzZWFyY2gtdGVzdC5uaWVocy5uaWguZ292JywgJ2VkaXQtdHN0J10sXG4gICAgZW52aXJvbm1lbnQ6ICdQcm9kJyxcbiAgICBQdWJsaWNTaXRlSG9zdE5hbWU6ICdodHRwczovL250cC5uaWVocy5uaWguZ292JyxcbiAgICBpc1Byb2R1Y3Rpb246IGZhbHNlXG59XG5cbi8vIGNoZWNrIHRvIHNlZSBpZiB3ZSdyZSBvbiBwcm9kdWN0aW9uXG5mb3IgKHZhciBpID0gMDsgaSA8IE5UUC5kZXZIb3N0TmFtZXMubGVuZ3RoOyBpKyspIHtcbiAgICBpZiAoTlRQLmFjdHVhbEhvc3RuYW1lID09PSBOVFAuZGV2SG9zdE5hbWVzW2ldKSB7XG4gICAgICAgIE5UUC5lbnZpcm9ubWVudCA9ICdEZXYnXG4gICAgICAgIE5UUC5QdWJsaWNTaXRlSG9zdE5hbWUgPSAnaHR0cHM6Ly9udHBkZXYubmllaHMubmloLmdvdidcbiAgICAgICAgYnJlYWs7XG4gICAgfVxufVxuZm9yICh2YXIgaSA9IDA7IGkgPCBOVFAudGVzdEhvc3ROYW1lcy5sZW5ndGg7IGkrKykge1xuICAgIGlmIChOVFAuYWN0dWFsSG9zdG5hbWUgPT09IE5UUC50ZXN0SG9zdE5hbWVzW2ldKSB7XG4gICAgICAgIE5UUC5lbnZpcm9ubWVudCA9ICdUZXN0J1xuICAgICAgICBOVFAuUHVibGljU2l0ZUhvc3ROYW1lID0gJ2h0dHBzOi8vbnRwdGVzdC5uaWVocy5uaWguZ292J1xuICAgICAgICBicmVhaztcbiAgICB9XG59XG5mb3IgKHZhciBpID0gMDsgaSA8IE5UUC5wcm9kdWN0aW9uSG9zdG5hbWVzLmxlbmd0aDsgaSsrKSB7XG4gICAgaWYgKE5UUC5hY3R1YWxIb3N0bmFtZSA9PT0gTlRQLnByb2R1Y3Rpb25Ib3N0bmFtZXNbaV0pIHtcbiAgICAgICAgTlRQLmVudmlyb25tZW50ID0gJ1Byb2QnXG4gICAgICAgIE5UUC5QdWJsaWNTaXRlSG9zdE5hbWUgPSAnaHR0cHM6Ly9udHAubmllaHMubmloLmdvdidcbiAgICAgICAgTlRQLmlzUHJvZHVjdGlvbiA9IHRydWVcbiAgICAgICAgYnJlYWs7XG4gICAgfVxufVxuXG4vLyBwcmV2ZW50IGNyb3NzLWZyYW1lIHNjcmlwdGluZyBieSBmb3JjaW5nIHRoZSBkb2N1bWVudCB0byBiZSB0aGUgdG9wIC0gb25seSBmb3IgcHJvZHVjdGlvbiBpbnN0YW5jZVxuaWYgKHRvcCAhPSBzZWxmKSB0b3AubG9jYXRpb24gPSBzZWxmLmxvY2F0aW9uO1xuXG4kKGZ1bmN0aW9uICgpIHtcbiAgICAvLyByZWdpc3RlciBwcmludCB0aGlzIHBhZ2UgZXZlbnQuXG4gICAgJChOVFAucHJpbnRwYWdlKS5hdHRyKCdocmVmJywgJyMnKS5jbGljayhmdW5jdGlvbiAoKSB7IHdpbmRvdy5wcmludCgpOyB9KTtcblxuICAgIC8vcmVnaXN0ZXIgcmVndWxhdG9yeSBhY3Rpb25zIGV2ZW50LlxuICAgICQoTlRQLnJhQWdlbmNpZXMpLmNoYW5nZShmdW5jdGlvbiAoKSB7IGxvY2F0aW9uLmhyZWYgPSBcIiNcIiArICQodGhpcykudmFsKCk7IH0pO1xufSk7XG5cbi8qICB0aGlzIHJlbW92ZXMgcGxhY2Vob2xkZXIgdGV4dCBiZWZvcmUgcHJpbnRpbmcgLSBBZGRlZCBieSBNYXJrIENvbGViYW5rIDUvOC8yMDE1IFxuY2FsbGVkIGluIGJvZHkgdGFnIG9uIGZvcm1zIDxib2R5IG9uYmVmb3JlcHJpbnQ9XCJyZW1vdmVQbGFjZWhvbGRlcigpXCI+ICovXG5mdW5jdGlvbiByZW1vdmVQbGFjZWhvbGRlcigpIHtcbiAgICAkKFwiaW5wdXRcIikucmVtb3ZlQXR0cihcInBsYWNlaG9sZGVyXCIpO1xuICAgICQoXCJ0ZXh0YXJlYVwiKS5yZW1vdmVBdHRyKFwicGxhY2Vob2xkZXJcIik7XG59XG5cbi8vYWRkcyBJRSBzdXBwb3J0IGZvciBpbmNsdWRlcygpIEpTIG1ldGhvZCAtIHNlZSBodHRwczovL2RldmVsb3Blci5tb3ppbGxhLm9yZy9lbi1VUy9kb2NzL1dlYi9KYXZhU2NyaXB0L1JlZmVyZW5jZS9HbG9iYWxfT2JqZWN0cy9TdHJpbmcvaW5jbHVkZXNcbmlmICghU3RyaW5nLnByb3RvdHlwZS5pbmNsdWRlcykge1xuICAgIFN0cmluZy5wcm90b3R5cGUuaW5jbHVkZXMgPSBmdW5jdGlvbigpIHtcbiAgICAgICAgJ3VzZSBzdHJpY3QnO1xuICAgICAgICByZXR1cm4gU3RyaW5nLnByb3RvdHlwZS5pbmRleE9mLmFwcGx5KHRoaXMsIGFyZ3VtZW50cykgIT09IC0xO1xuICAgIH07XG59XG5cbi8vZGVmaW5lIGV4dGVybmFsIGxpbmsgZGlzY2xhaW1lciB0aXRsZSBhdHRyaWJ1dGU7IHVzZWQgd2l0aGluIG50cC5mb290ZXIuanMgYW5kIG50cC5leHRlcm5hbC1saW5rcy5qc1xudmFyIG50cEV4dGVybmFsRGlzY2xhaW1lciA9ICdUaGlzIGxpbmsgaXMgdG8gYSBub24tTlRQIHdlYnNpdGUuIExpbmtzIGRvIG5vdCBjb25zdGl0dXRlIGVuZG9yc2VtZW50IGJ5IE5UUCBvZiB0aGUgbGlua2VkIHdlYnNpdGUuIFZpc2l0b3JzIHRvIHRoZSBsaW5rZWQgd2Vic2l0ZSB3aWxsIGJlIHN1YmplY3QgdG8gdGhlIHdlYnNpdGUgcHJpdmFjeSBwb2xpY2llcy4gVGhlc2UgcHJhY3RpY2VzIG1heSBiZSBkaWZmZXJlbnQgdGhhbiB0aG9zZSBvZiB0aGlzIE5UUCB3ZWJzaXRlLic7XG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT1cbi8vIEZvb3RlciAocmVxdWlyZXMgalF1ZXJ5LCBudHAuY29tbW9uLmpzLCAmIEZvdW5kYXRpb24gTWVkaWFRdWVyeSlcbi8vID09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PVxuJChkb2N1bWVudCkucmVhZHkoZnVuY3Rpb24oKXtcblxuXHRsZXQgZm9vdGVySGVhZGluZ3MgPSAkKCcubGluay1jYXRlZ29yeSBoNSwgLmxpbmstY2F0ZWdvcnkgLmg1Jyk7XG5cblx0ZnVuY3Rpb24gZm9vdGVyTW9iaWxlKCkge1xuXHRcdCQoJy5saW5rLWNhdGVnb3J5IHVsJykuaGlkZSgpO1xuXHRcdGZvb3RlckhlYWRpbmdzLnVuYmluZCgnY2xpY2sga2V5dXAnKS5hdHRyKCd0YWJpbmRleCcsICcwJykub24oe1xuXHRcdFx0Y2xpY2s6IGZ1bmN0aW9uKCkge1xuXHRcdFx0XHQkKHRoaXMpLm5leHQoJ3VsJykuc2xpZGVUb2dnbGUoKTtcblx0XHRcdFx0JCh0aGlzKS50b2dnbGVDbGFzcygnZm9vdGVyLW5hdi1hY3RpdmUnKTtcblx0XHRcdH0sXG5cdFx0XHRrZXl1cDogZnVuY3Rpb24oZXZlbnQpIHtcblx0XHRcdFx0aWYoZXZlbnQua2V5Q29kZSA9PSAxMyl7XG5cdFx0XHRcdFx0JCh0aGlzKS5jbGljaygpO1xuXHRcdFx0XHR9XG5cdFx0XHR9XG5cdFx0fSk7XG5cdH1cblx0ZnVuY3Rpb24gZm9vdGVyRGVza3RvcCgpIHtcblx0XHRmb290ZXJIZWFkaW5ncy5yZW1vdmVBdHRyKCd0YWJpbmRleCcpLm9mZignY2xpY2sga2V5dXAnKS5yZW1vdmVDbGFzcygnZm9vdGVyLW5hdi1hY3RpdmUnKTtcblx0XHQkKCcubGluay1jYXRlZ29yeSB1bCcpLnNob3coKTtcblx0fVxuXG5cdC8vdHJpZ2dlciBhdCBicmVha3BvaW50XG5cdG9uRm9vdGVyTW9iaWxlID0gZnVuY3Rpb24oKSB7XG5cdFx0aWYgKEZvdW5kYXRpb24uTWVkaWFRdWVyeS5pcygnc21hbGwgb25seScpKSB7XG5cdFx0XHRmb290ZXJNb2JpbGUoKTtcblx0XHR9IGVsc2Uge1xuXHRcdFx0Zm9vdGVyRGVza3RvcCgpO1xuXHRcdH1cblx0fVxuXG5cdC8vb24gcGFnZSBsb2FkIGFuZCBtZWRpYSBxdWVyeSBjaGFuZ2UsIGluaXRpYWxpemUgb25Gb290ZXJNb2JpbGUoKTtcblx0JCh3aW5kb3cpLm9uKCdsb2FkIGNoYW5nZWQuemYubWVkaWFxdWVyeScsIG9uRm9vdGVyTW9iaWxlKTtcblxuXHQvL2FkZCB0aXRsZSBhdHRyaWJ1dGUgdG8gZXh0ZXJuYWwgbGlua3MgaW4gdGhlIGZvb3RlclxuXHQkKCcjZm9vdGVyIGEuZXh0ZXJuYWwsIC5mb290ZXItbG9nb3MgYScpLmF0dHIoJ3RpdGxlJywgbnRwRXh0ZXJuYWxEaXNjbGFpbWVyKTtcblxufSk7XG5cInVzZSBzdHJpY3RcIjtcbi8qanNsaW50IGJyb3dzZXI6IHRydWUqL1xuLypnbG9iYWwgICQqL1xuLy8gSmF2YXNjcmlwdCBmb3IgVGh1bmRlcnN0b25lIHNlYXJjaFxuJChmdW5jdGlvbigpIHtcblx0Ly8gZGVmYXVsdCBpZCB0byBxdWVyeVxuXHR2YXIgaWRzID0gJCgnW2lkXj1xdWVyeV0nKS5maWx0ZXIoXCIucXVlcnlBdXRvY29tcGxldGVcIik7XHRcbiAgICAvLyBhZGQgYXV0b2NvbXBsZXRlXG5cdGZvcih2YXIgaSA9MDtpPGlkcy5sZW5ndGggO2krKylcblx0e1xuXHRcdHZhciBpZCA9JCgnW2lkXj1xdWVyeV0nKS5maWx0ZXIoXCIucXVlcnlBdXRvY29tcGxldGVcIilbaV0uaWQ7XG5cdFx0YXV0b0NvbXBsZXRlKGlkKTtcblx0fVxuXHRmdW5jdGlvbiBhdXRvQ29tcGxldGUoaWQpXG5cdHtcblx0XHQkKFwiI1wiICsgaWQgKyBcIi5xdWVyeUF1dG9jb21wbGV0ZVwiKS5hdXRvY29tcGxldGUoe1xuXHRcdFx0c291cmNlOiBmdW5jdGlvbihyZXF1ZXN0LCByZXNwb25zZSkge1xuXHRcdFx0XHR2YXIgYWN0aW9uID0gJChcIiNcIitpZCtcIlwiKS5jbG9zZXN0KFwiZm9ybVwiKS5hdHRyKFwiYWN0aW9uXCIpO1xuXHRcdFx0XHR2YXIgdXJsID0gXCIvL3NlZWsubmllaHMubmloLmdvdi90ZXhpcy9hdXRvY29tcGxldGUuanNvblwiO1xuXHRcdFx0XHR2YXIgcHJvZmlsZSA9IFwibnRwXCI7XG5cdFx0XHRcdCQuYWpheCh7XG5cdFx0XHRcdFx0ZGF0YVR5cGU6IFwianNvbnBcIixcblx0XHRcdFx0XHR1cmw6IHVybCxcblx0XHRcdFx0XHRkYXRhOiB7XG5cdFx0XHRcdFx0XHRwcjogcHJvZmlsZSxcblx0XHRcdFx0XHRcdHRlcm06IHJlcXVlc3QudGVybVxuXHRcdFx0XHRcdH0sXG5cdFx0XHRcdFx0c3VjY2VzczogZnVuY3Rpb24oZGF0YSkge1xuXHRcdFx0XHRcdFx0cmVzcG9uc2UoZGF0YS5jb21wbGV0aW9ucyk7XG5cdFx0XHRcdFx0fVxuXHRcdFx0XHR9KTtcblx0XHRcdH0sXG5cdFx0XHRtaW5MZW5ndGg6IDIsXG5cdFx0XHQvL29wZW46IGZ1bmN0aW9uKGUsIHVpKSB7XG5cdFx0XHRcdC8vIGRyaWxsIGludG8gdGhlIG1lbnUgYW5kIHdyYXAgdGhlIHRlcm0gYW5kIHRoZSBjb21wbGV0aW9uc1xuXHRcdFx0XHQvLyBpbiBzcGFucyBmb3Igc3R5bGluZy4gIEhvcGVmdWxseSBKUXVlcnlVSSB3aWxsIHN0YXJ0IGRvaW5nIHRoaXNcblx0XHRcdFx0Ly8gYXV0b21hdGljYWxseSBpbiB0aGUgZnV0dXJlLlxuLy9cdFx0XHRcdHZhciB0ZXJtID0gJChcIiNcIitpZCtcIlwiKS52YWwoKTtcbi8vXHRcdFx0XHR2YXIgYWNEYXRhID0gJCh0aGlzKS5kYXRhKFwidWlBdXRvY29tcGxldGVcIik7XG4vL1x0XHRcdFx0YWNEYXRhLm1lbnUuZWxlbWVudC5maW5kKFwiYVwiKS5lYWNoKGZ1bmN0aW9uKCkge1xuLy9cdFx0XHRcdFx0dmFyIGEgPSAkKHRoaXMpO1xuLy9cdFx0XHRcdFx0dmFyIGNvbXBsZXRpb24gPSBhLnRleHQoKTtcbi8vXHRcdFx0XHRcdHZhciBwb3MgPSBjb21wbGV0aW9uLmluZGV4T2YodGVybSk7XG4vL1x0XHRcdFx0XHR2YXIgdHh0ID0gXCJcIjtcbi8vXHRcdFx0XHRcdGlmKHBvcyAhPT0gMCkge1xuLy9cdFx0XHRcdFx0XHR0eHQgKz0gXCI8c3BhbiBjbGFzcz0ndWktYXV0b2NvbXBsZXRlLWNvbXBsZXRpb24nPlwiICsgY29tcGxldGlvbi5zdWJzdHIoMCwgcG9zKSArIFwiPC9zcGFuPlwiO1xuLy9cdFx0XHRcdFx0fVxuLy9cdFx0XHRcdFx0dHh0ICs9IFwiPHNwYW4gY2xhc3M9J3VpLWF1dG9jb21wbGV0ZS10ZXJtJz5cIiArIHRlcm0gKyBcIjwvc3Bhbj5cIjtcbi8vXHRcdFx0XHRcdGlmKHBvcyArIHRlcm0ubGVuZ3RoIDwgY29tcGxldGlvbi5sZW5ndGgpIHtcbi8vXHRcdFx0XHRcdFx0dHh0ICs9IFwiPHNwYW4gY2xhc3M9J3VpLWF1dG9jb21wbGV0ZS1jb21wbGV0aW9uJz5cIiArIGNvbXBsZXRpb24uc3Vic3RyKHBvcyArIHRlcm0ubGVuZ3RoKSArIFwiPC9zcGFuPlwiO1xuLy9cdFx0XHRcdFx0fVxuLy9cdFx0XHRcdFx0YS5odG1sKHR4dCk7XG4vL1x0XHRcdFx0fSk7XG4vL1x0XHRcdH0sXG5cdFx0XHRzZWxlY3Q6IGZ1bmN0aW9uKGV2ZW50LCB1aSkge1xuXHRcdFx0XHQkKFwiI1wiK2lkK1wiXCIpLnZhbCh1aS5pdGVtLnZhbHVlKS5jbG9zZXN0KFwiZm9ybVwiKS5zdWJtaXQoKTtcblx0XHRcdH1cblx0XHR9KTtcblx0fVxufSk7XG5cbi8vID09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PVxuLy8gVS5TLiBHb3Zlcm5tZW50IGhlYWRlciBhY2NvcmRpb24gdG9nZ2xlXG4vLyA9PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT09PT1cbiQoZG9jdW1lbnQpLnJlYWR5KGZ1bmN0aW9uKCkge1xuICAgIHZhciBlID0gJChcIi51c2EtY2xvc2VkXCIpO1xuICAgICQoXCIudXNhLWFjY29yZGlvbi1idXR0b25cIikub24oXCJjbGlja1wiLCBmdW5jdGlvbih0KSB7XG4gICAgICAgICQoZSkuaGFzQ2xhc3MoXCJ1c2EtY2xvc2VkXCIpID8gKCQodGhpcykuYXR0cihcImFyaWEtZXhwYW5kZWRcIiwgXCJ0cnVlXCIpLCAkKGUpLnJlbW92ZUNsYXNzKFwidXNhLWNsb3NlZFwiKSwgJChlKS5hZGRDbGFzcyhcInVzYS1vcGVuXCIpKSA6ICgkKHRoaXMpLmF0dHIoXCJhcmlhLWV4cGFuZGVkXCIsIFwiZmFsc2VcIiksICQoZSkuYWRkQ2xhc3MoXCJ1c2EtY2xvc2VkXCIpLCAkKGUpLnJlbW92ZUNsYXNzKFwidXNhLW9wZW5cIikpXG4gICAgfSlcbn0pOyJdLCJmaWxlIjoibnRwd2ViLXNoYXJlZC1taW5pbWFsLmpzIn0=
