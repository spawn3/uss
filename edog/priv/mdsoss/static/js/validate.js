
   
	function insertAfter(newElement,targetElement){
		var parent=targetElement.parentNode;
		if(parent.nextSibling==targetElement){
			parent.appendChild(newElement);
		}
		else{
			parent.insertBefore(newElement,targetElement.nextSibling);
		}
	}





   function check(name,value,reg,erro_info)
   {
   		var flag=true;
		var span;
		flag=reg.test(value);	
		if(flag == true)
		{	
			var span=document.getElementById(name+"_error");
			if(span != null)
			{   
				span.parentNode.removeChild(span);
			}
   			return true;
		}
   		else 
   		{		
			if(document.getElementById(name+"_error")==null)
			{
			span = document.createElement("span");
			span.id=name+"_error";
			span.style.color="red";
			span.style.fontSize="11px";
			span.textContent=erro_info;
			curNode = document.getElementsByName(name).item(0);
			insertAfter(span,curNode);
			}
   			return false;
   		}
   }




   function checkName(name)
   {
   		var reg=/^[a-zA-Z0-9]{5,15}$/;
		var error_info="标识名称必须大于5字符，并小于15个字符";
		return check(name,reg,error_info);
   }
   
   function checkPasswd(passwd)
   {
		var reg=/^[\w]{6,12}$/;
		var error="密码必须大于6字符小于12字符，且自包含字母，数字和下划线";
		return check("passwd",passwd,reg,error);
   }
   

   function checkTelphone(telephone)
   {

   		var reg=/^[0-9]{3,4}\-[0-9]{7,8}$/;
		var error ="请按格式输入有效地固定电话号码";
		return check("telephone",telephone,reg,error);
   }   
   
   
	function checkCellphone(cellphone)
	{

		var reg=/^[0-9]{11}$/;
		var error = "请输入有效的移动电话";
		return check("cellphone",cellphone,reg,error);
   }
   
    function checkEmail(email)
   {
		var reg = /^([a-zA-Z0-9_-])+@([a-zA-Z0-9_-])+((\.[a-zA-Z0-9_-]{2,3}){1,2})$/;
		var error="请输入有效的email地址";
		return check("email",email,reg,error);

   }
   
   function checkCompany(company)
   {
		//var reg = /^\w{3,30}$/;
		//var error ="标识符必须大于3个字符，并小于30个字符";
		//return check("company",company,reg,error);
			return true;
   }
   
   
     function checkCompanyAlias(alias)
   {
	   var error="标识符必须大于3个字符，并小于15个字符";
	   var reg = /^\w{3,30}$/;
 	   return check("alias",alias,reg,error);
   }
   
   
     function checkContact_person(contact_person)
   {
		//var reg = /^\w{3,15}$/;
		//var error ="标识符必须大于3个字符，并小于15个字符";
		//return check("contact_person",contact_person,reg,error);
			return true;
   }
   
     function checkAddress(address)
   {
		//var reg = /^\w{3,15}$/;
		//var error ="请详实您的联系地址以便于我们联系您";
      //return check("address",address,reg,error);
			//
			return true;
   }
   
    function checkCpu(cpu_cores)
	{
		var reg = /^[1-4]{1}$/;
		var error = "CPU内核个数必须大于等于1，小于等于4,并且为数字";
		return check("cpu_cores",cpu_cores,reg,error);
	}

  function checkMem(mem_capacity)
	{
		var reg = /^[1-4]{1}$/;
		var error = "内存容量必须大于等于1，小于等于4,并且为数字";
		return check("mem_capacity",mem_capacity,reg,error);
	}

  function checkVirtualNetworkCard_num(virtualNetworkCard_num)
	{
		var reg = /^[1-4]{1}$/;
		var error = "虚拟网卡个数必须小于等于4,并且为数字";
		return check("virtualNetworkCard_num",virtualNetworkCard_num,reg,error);
	}

  function checkVm_name(vm_name)
	{
		var reg = /^\w{3,15}$/;
		var error = "标识名称必须大于3，并小于15个字符";
		return check("vm_name",vm_name,reg,error);
	}

  function submitCustormer()
  {
	  var alias = document.getElementById("alias").value;
	  var company = document.getElementById("company").value;	  
	  var address = document.getElementById("address").value;
	  var telephone = document.getElementById("telephone").value;	  
	  var contact_person = document.getElementById("contact_person").value;	  
	  var cellphone = document.getElementById("cellphone").value;	
	  var email = document.getElementById("email").value;		 
	  
	  if(checkCompanyAlias(alias)&&checkCompany(company)&&checkAddress(address)&&checkTelphone(telephone)&&checkContact_person(contact_person)&&checkCellphone(cellphone)&&checkEmail(email))
	  {
		  return true;
	  }  
	  return false;

  }
  
  function submitVM()
  {
	  var cpu_cores = document.getElementById("cpu_cores").value;
	  var mem_capacity = document.getElementById("mem_capacity").value;	 
	  var virtualNetworkCard_num = document.getElementById("virtualNetworkCard_num").value;			
	  var vm_name = document.getElementById("vm_name").value;	
	  
	  if(checkCpu(cpu_cores)&&checkMem(mem_capacity)&&checkVirtualNetworkCard_num(virtualNetworkCard_num)&&checkVm_name(vm_name))
	  {

		  return true;
	  }	  
	  return false;  
  }
