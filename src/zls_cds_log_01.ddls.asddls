@AbapCatalog.sqlViewName: 'ZSQL_LOG_01'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'output alv ida'
define view zls_cds_log_01 
  as select from balhdr as bal
    left outer join zls_t_log  as head on 
             head.extnumber = bal.extnumber
//             head.name = 'TAG' and
//             head.value = 'TAG'
                                  
//    inner join balhdr    as bal on  bal.object    = '/ZRG/LOG'
//                                  and bal.extnumber = head.extnumber
{

 key bal.lognumber,
 
 cast( case when bal.msg_cnt_e is not initial then '@5C@'
  when bal.msg_cnt_w is not initial then '@5D@'
  when bal.msg_cnt_s is not initial then '@5B@'
  else '' end as abap.char(4) )  as type_icon,

    aldate,
    altime,
//  bal.altext                 as description,
    bal.altcode                as tcode,
    bal.aluser,
    bal.object,
    bal.subobject,

 case when bal.msg_cnt_e is not initial then 'E'
  when bal.msg_cnt_w is not initial then 'W'
  when bal.msg_cnt_s is not initial then 'S'
  else '' end  as type,

//head.parname,
//head.parvalue,
  head.extnumber,
  head.tagdata,
  head.tagmsg,
  head.tagstack

//  case when bal.msg_cnt_e is not initial then 'E'
//  when bal.msg_cnt_w is not initial then 'W'
//  when bal.msg_cnt_s is not initial then 'S'
//  else '' end as type,
//    aldate,
//  altime,
//  bal.altext                 as description,
//  bal.altcode                as tcode,
//  bal.aluser,
// key head.arbgb,
// key head.msgnr,
// key head.sprsl,
// head.text
////  ltrim( bal.lognumber , '0' ) as lognumber

}

//group by head.extnumber
