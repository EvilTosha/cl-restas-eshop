﻿
/* - - - - - - - - - - - -
 * Creator: style-nes
 * URL: style-nes.com
 * Email: style-nes@ya.ru
 * - - - - - - - - - - - - */	
 
 
 function order_now(href){
		var noerrors = true;
		$(document).find("input.required, textarea.required").each(function(){
			if ($(this).val().toString() === '') {
				if ((!$(this).hasClass("step_area")) || $("div.payment_method .item :checked").attr("id") == "payment_method-4"){
					if (!$(this).parent().hasClass('error')) {
						$(this).parent().addClass('error');
						$("<br><label class='errorlabel' style='color: red;'>Не заполнено обязательное поле</label>").insertAfter(this);
					}
					noerrors = false;
				}
			} 
			else {
				$(this).parent().removeClass('error').find('.errorlabel').remove();
			}
		});
		if (!noerrors) {
			$(document).find('.error').eq(0).find(':input').focus();
		} 
		else {
			obj = new Object;
			$(document).find('input, textarea').each(function(){
				var name = $(this).attr('id');
				var val = $(this).val();
				if ($(this).is(":checkbox")) {
					obj[name]=$(this).is(":checked");
				} 
				else if ($(this).is(":radio")) {
					if ($(this).is(":checked")) {
						obj[$(this).attr("name")] = $(this).attr("id");
					}
				}
				else{
					obj[name] = val;
				}
				if ($("#show_express").hasClass('none'))
					obj["delivery-type"] = "express";
				else
					obj["delivery-type"] = 'pickup';
				
			});
			$(document).find("select").each(function(){
				var name = $(this)[0].id;
				obj[name] = $(this)[0].options[$(this)[0].selectedIndex].value;
				
			});
			$(document).cookie('user-nc', JSON.stringify(obj), {path: "/"});
			window.location = href;
		}
	}
	

function setUserCookieField(fieldName, val){
	var user = eval("(" + $(document).cookie('user-nc') + ")");
	if (!user){
		user = new Object;
	}
	user[fieldName] = val;
	$(document).cookie('user-nc', JSON.stringify(user), {path: "/"});
	
}
	
 
$(document).ready(function(){

	$(".item#beznalichnii .price").html(Math.ceil(parseInt($("span.price").text()) * 0.01) + ' рублей');

	// Показать больше информации
	$('div.delivery a.show_hidden').toggle(
		function() {
			$(this).parent().find(".hide").show("normal");
			$(this).html($(this).html().replace('Подробнее', 'Короче'));
			return false;
		}, 
		function() {
			$(this).parent().find(".hide").hide("normal");
			$(this).html($(this).html().replace('Короче', 'Подробнее'));
			return false;
	});
	
	
	// Доставка / Забрать самостоятельно
	$("a#show_pickup").click(function() {
		$("div.delivery .pickup").show();
		$("div.delivery .express").hide();

		$(this).addClass('none');
		$("#show_express").removeClass('none');
		
		$(".item#dostavka").hide();
		
		var user = eval('(' + $(document).cookie('user-nc') +')');
		var newPrice = parseInt($("span.price").text()) - 300;
		if (user && user['payment'] == 'payment_method-4'){
			newPrice -= 3;
		}
		$(".item#beznalichnii .price").html((parseInt($(".item#beznalichnii .price").text()) - 3) + ' рублей');
		
		$("span.price").html(newPrice);
		
		setUserCookieField("delivery-type", "pickup");
		
		return false;
	});
	
	
	// Доставка / Доставить курьером
	$("a#show_express, a#express").click(function() {
		$("div.delivery .express").show();
		$("div.delivery .pickup").hide();
		
		$("a#show_express").addClass('none');
		$("#show_pickup").removeClass('none');
		
		$(".item#dostavka").show();
		
		
		var user = eval('(' + $(document).cookie('user-nc') +')');
		var newPrice = parseInt($("span.price").text()) + 300;
		if (user && user['payment'] == 'payment_method-4'){
			newPrice += 3;
		}
		$(".item#beznalichnii .price").html((parseInt($(".item#beznalichnii .price").text()) + 3) + '  рублей');
		$("span.price").html(newPrice);
		
		setUserCookieField("delivery-type", "express");
		
		return false;
	});
	
	
	// Доставка / Карта проезда
	$('div.delivery a.show_hidden').toggle(
		function() {
			$(this).parent().find(".hide").show("normal");
			$(this).html($(this).html().replace('Карта проезда', 'Скрыть карту проезда'));
			return false;
		}, 
		function() {
			$(this).parent().find(".hide").hide("normal");
			$(this).html($(this).html().replace('Скрыть карту проезда', 'Карта проезда'));
			return false;
	});

	
	// Оплата заказа
	$("div.payment_method .item").click(function() {
				
		var id = $(this).find("input").attr("id");
		$("div.method_desc div").hide();
		$("div.method_desc .more").show();
		$("div.method_desc div."+id).show();
		
		var user = eval('(' + $(document).cookie('user-nc') + ')');
		if (id == "payment_method-4" && !($(this).hasClass('active'))){
			
			$(".item#beznalichnii").show();
			var newPrice = parseInt($("span.price").text()) + parseInt($(".item#beznalichnii .price").text());
			
			$("span.price").html(newPrice);
		}
		if (id != "payment_method-4" && (!user || user["payment"] == "payment_method-4")){
			$(".item#beznalichnii").hide();
			var newPrice = parseInt($("span.price").text()) - parseInt($(".item#beznalichnii .price").text());
			
			$("span.price").html(newPrice);
		}
		
		$(this).parent().find(".item").removeClass("active");
		$(this).find("input").attr("checked", "checked");
		$(this).addClass("active border5");
		
		setUserCookieField("payment", id);
	});
	
	//Выбор магазина
	$("div.pickup :radio").click(function() {
		var id = $(this).attr("id");
		setUserCookieField("pickup", id);
	});
	
	$("input#discount-card").click(function(){
		setUserCookieField($(this).attr("id"), $(this).is(":checked"));
	});
	
	// Есть дисконт
	$("input#discount-card").click(function() {
		$(this).parent().parent().find(".hide").toggle("normal");
	});
	
	
	//Нажатие на "оформить заказ"
	$("a[rel='submit-cookie']").click(function(){
		order_now(this);
		return false;
	});
	
	/*Изменение куков при потере фокуса ввода*/
	$("input, textarea").blur(function(){
		var user = eval('(' + $(document).cookie('user-nc') + ')');
		var name = $(this).attr('id');
		var val = $(this).val();
                // костыль
                if (!user){
		    user = new Object;
	        }
		if ($(this).is(":checkbox")) {
			user[name]=$(this).is(":checked");
		} 
		else if ($(this).is(":radio")) {
			if ($(this).is(":checked")) {
				user[$(this).attr("name")] = $(this).attr("id");
			}
		}
		else{
			user[name] = val;
		}
		$(document).cookie('user-nc', JSON.stringify(user), {path: "/"});		
	});
	
	
	/*Инициализация пользовательских данных*/
	if (document.getElementById('discount-card')){
		document.getElementById('discount-card').checked = false;
	}
	var user = eval('(' + $(document).cookie('user-nc') +')');
	var oldUser = eval('(' + $(document).cookie('user') +')');
	if (user){
		if (user['discount-card'] && document.getElementById('discount-card')){
			document.getElementById('discount-card').click();
		}
		if (user['pickup'] && document.getElementById(user['pickup'])){
			document.getElementById(user['pickup']).click();
		}
		if (user['payment'] && document.getElementById(user['payment'])){
			document.getElementById(user['payment']).click();
		}
		if (user['delivery-type'] == 'express'){
			if ($("a#show_express")[0]){
				$("a#show_express").click();
			}
			else{
				if ($(".item#dostavka")){
					$(".item#dostavka").show();
				}
				if ($("span.price")){
					var newPrice = parseInt($("span.price").text()) + 300;
					$("span.price").html(newPrice);
				}
			}
		}
		
		var ids = ['phone', 'name', 'addr', 'email', 'pickup_comment', 'courier_comment', 'discount-card-number', 'bankaccount'];
		for (var i = 0, len = ids.length; i < len; ++i){
			if (user[ids[i]] && document.getElementById(ids[i])){
				document.getElementById(ids[i]).value = user[ids[i]];
			}
		}
	}
	if (!user && oldUser){
		if (oldUser.auth){
			if (document.getElementById('name')){
				document.getElementById('name').value = oldUser.auth.name;
			}
			if (document.getElementById('email')){
				document.getElementById('email').value = oldUser.auth.email;
			}
			if (document.getElementById('phone')){
				document.getElementById('phone').value = oldUser.auth.phone;
			}
		}
		if (oldUser.delivery){
			if (document.getElementById('addr')){
				document.getElementById('addr').value = oldUser.delivery.addr;
			}
		}
		if (oldUser.pay){
			if (document.getElementById('bankaccount')){
				document.getElementById('bankaccount').value = oldUser.pay.bankaccount;
			}
		}
		$(document).cookie('user', '', { expires: -1 });
	}
	$('.checkout-thanks').each(function(){
		$(document).cookie('cart', '', { expires: -1 });
	});
});