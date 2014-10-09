
   
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
		var error_info="��ʶ���Ʊ������5�ַ�����С��15���ַ�";
		return check(name,reg,error_info);
   }
   
   function checkPasswd(passwd)
   {
		var reg=/^[\w]{6,12}$/;
		var error="����������6�ַ�С��12�ַ������԰�����ĸ�����ֺ��»���";
		return check("passwd",passwd,reg,error);
   }
   

   function checkTelphone(telephone)
   {

   		var reg=/^[0-9]{3,4}\-[0-9]{7,8}$/;
		var error ="�밴��ʽ������Ч�ع̶��绰����";
		return check("telephone",telephone,reg,error);
   }   
   
   
	function checkCellphone(cellphone)
	{

		var reg=/^[0-9]{11}$/;
		var error = "��������Ч���ƶ��绰";
		return check("cellphone",cellphone,reg,error);
   }
   
    function checkEmail(email)
   {
		var reg = /^([a-zA-Z0-9_-])+@([a-zA-Z0-9_-])+((\.[a-zA-Z0-9_-]{2,3}){1,2})$/;
		var error="��������Ч��email��ַ";
		return check("email",email,reg,error);

   }
   
   function checkCompany(company)
   {
		//var reg = /^\w{3,30}$/;
		//var error ="��ʶ���������3���ַ�����С��30���ַ�";
		//return check("company",company,reg,error);
			return true;
   }
   
   
     function checkCompanyAlias(alias)
   {
	   var error="��ʶ���������3���ַ�����С��15���ַ�";
	   var reg = /^\w{3,30}$/;
 	   return check("alias",alias,reg,error);
   }
   
   
     function checkContact_person(contact_person)
   {
		//var reg = /^\w{3,15}$/;
		//var error ="��ʶ���������3���ַ�����С��15���ַ�";
		//return check("contact_person",contact_person,reg,error);
			return true;
   }
   
     function checkAddress(address)
   {
		//var reg = /^\w{3,15}$/;
		//var error ="����ʵ������ϵ��ַ�Ա���������ϵ��";
      //return check("address",address,reg,error);
			//
			return true;
   }
   
    function checkCpu(cpu_cores)
	{
		var reg = /^[1-4]{1}$/;
		var error = "CPU�ں˸���������ڵ���1��С�ڵ���4,����Ϊ����";
		return check("cpu_cores",cpu_cores,reg,error);
	}

  function checkMem(mem_capacity)
	{
		var reg = /^[1-4]{1}$/;
		var error = "�ڴ�����������ڵ���1��С�ڵ���4,����Ϊ����";
		return check("mem_capacity",mem_capacity,reg,error);
	}

  function checkVirtualNetworkCard_num(virtualNetworkCard_num)
	{
		var reg = /^[1-4]{1}$/;
		var error = "����������������С�ڵ���4,����Ϊ����";
		return check("virtualNetworkCard_num",virtualNetworkCard_num,reg,error);
	}

  function checkVm_name(vm_name)
	{
		var reg = /^\w{3,15}$/;
		var error = "��ʶ���Ʊ������3����С��15���ַ�";
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
