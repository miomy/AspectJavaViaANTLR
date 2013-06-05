grammar OBAspect;

options {
    backtrack=true;
    memoize=true;
    output=template;
}

tokens {
    ASP = 'aspect';
    PCD = 'pointcut';
    ADV = 'advice';
    RUL = '@';
    TKN = '#';
    ATR = '$';

    BEF = 'before';
    AFT = 'after';
    MTH = 'match';
    SET = 'set';

    WTN = 'within';
    CFW = 'cflow';
    CFB = 'cflowbelow';
    CFP = 'cflowpattern';

    NOT = '!';
    OR  = '||';
    AND = '&&';

    GET_PARSED_TEXT = 'GetParsedText';
    GET_TOKEN = 'GetToken';
    GET_PARSING_STACK_TRACE = 'GetParsingStackTrace';
    GET_BACKTRACK_LEVEL = 'GetBacktrackLevel';

    BEFORE_MAIN_EXIT = 'BeforeMainExit';
}

@members {
    public HashMap<String,String> pointcutDescriptorMap;
    public String current_cflowpattern = "";
}

program 
    :   pd=packageDeclaration ad=aspect_declaration EOF
        -> program(packageDeclaration={$pd.st}, aspectDeclaration={$ad.st})
    ;

packageDeclaration
    :   PACKAGE qn=qualifiedName SEMI
        -> pckDecl(packageKeyword={$PACKAGE.text}, packageName={$qn.text})
    ;

aspect_declaration
    :   am=access_modifier ASP an=IDENTIFIER LPAREN grn=IDENTIFIER RPAREN LBRACE asl=aspect_statement_list RBRACE
        -> asptDecl(accessModifier={$am.code}, aspectName={$an.text}, grammarName={$grn.text}, aspectBody={$asl.st})
    ;

access_modifier returns [String code] 
    :   PUBLIC {$code = $PUBLIC.text;}
    |   PROTECTED {$code = $PROTECTED.text;}
    |   PRIVATE  {$code = $PRIVATE.text;}
    |   {$code = "private";}
    ;

aspect_statement_list
    :   (s+=aspect_statement)+ -> lineList(l={$s})
    ;

aspect_statement
    :   md=memberDecl
        -> copyStr(f={$md.st})
    |   pd=pcd_decl
        -> copyStr(f={$pd.st})
    |   ad=adv_decl
        -> copyStr(f={$ad.st})
    ;
    
pcd_decl
    :   am=access_modifier pd=pointcut_descriptor SEMI
        -> pcdDecl(accessModifier={$am.code}, pointcutDescriptor={$pd.st})
    ;

pointcut_descriptor
    :   PCD IDENTIFIER EQ p=pcd_body
        -> pointcutDescriptor(pcdKeyword={$PCD.text}, pcdName={$IDENTIFIER.text}, pcdBody={$p.st})
    ;

pcd_body
    :   df=disflt -> domainSpecificPCD(pcd={$df.st})
    |   mf=macro_filter -> copyStr(f={$mf.st})
    ;

adv_decl
    :   ADV COLON d=disflt b=block
        -> advDecl(locMod={"before"}, boundPCD={$d.st}, customCodeBlock={$b.st})
    |   lm=location_modifier COLON d=disflt b=block
        -> advDecl(locMod={$lm.text}, boundPCD={$d.st}, customCodeBlock={$b.st})
    ;

disflt
    :   c=conflt d=disflt_f -> disFlt(con={$c.st}, disf={$d.st})
    ;

disflt_f
    :   OR d=disflt -> disFltf(dis={$d.st})
    |   -> eptStr()
    ;

conflt
    :   l=litflt c=conflt_f -> conFlt(lit={$l.st},conf={$c.st})
    ;

conflt_f
    :   AND c=conflt -> conFltf(con={$c.st})
    |   -> eptStr()
    ;

litflt
    :   LPAREN d=disflt RPAREN -> wrapStr(f={$d.st})
    |   af=atomic_filter -> copyStr(f={$af.st})
    |   NOT lf=litflt -> tglFlt(f={$lf.st})
    ;

atomic_filter
    :   pf=pointcut_filter -> atmFlt(f={$pf.st})
    |   abf=advice_bound_filter -> copyStr(f={$abf.st})
    ;

pointcut_filter
    :   pf=pinpoint_filter -> copyStr(f={$pf.st})
    |   rf=range_filter -> copyStr(f={$rf.st})
    ;

pinpoint_filter
    :   af=attribute_filter -> copyStr(f={$af.st})
    |   tf=token_filter -> copyStr(f={$tf.st})
    |   nf=nonterminel_filter -> copyStr(f={$nf.st})
    |   bf=branch_filter -> copyStr(f={$bf.st})
    ;

macro_filter
    :   PERCENT m=macro_filter_expression
        -> copyStr(f={$m.st})
    ;

macro_filter_expression
    :   b=before_main_exit
        -> copyStr(f={$b.st})
    ;

before_main_exit
    :   BEFORE_MAIN_EXIT
        -> beforeMainExit()
    ;


attribute_filter
    :   lm=location_modifier LPAREN rn=rule_indicator ai=alt_indicator_for_within COLON an=attr_indicator RPAREN
        -> attrFilter1(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, alt={$ai.st}, attributeName={$an.code})
    |   lm=location_modifier LPAREN rn=rule_indicator ai=alt_indicator_for_within COLON NOT an=attr_indicator RPAREN
        -> attrFilter2(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, alt={$ai.st}, attributeName={$an.code})
    |   lm=location_modifier LPAREN NOT rn=rule_indicator ai=alt_indicator_for_within COLON an=attr_indicator RPAREN
        -> attrFilter3(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={"\\\\w*"}, alt={$ai.st}, attributeName={$an.code})
    |   lm=location_modifier LPAREN an=attr_indicator RPAREN
        -> attrFilter1(locationModifier={$lm.code}, ruleNameValue={"*"}, alt={""}, attributeName={$an.code})
    |   lm=location_modifier LPAREN NOT an=attr_indicator RPAREN
        -> attrFilter2(locationModifier={$lm.code}, ruleNameValue={"*"}, alt={""}, attributeName={$an.code})
    ;

location_modifier returns [String code] 
    :   BEF {$code = "Begin";}
    |   AFT {$code = "End";}
    ;

rule_indicator returns [String value, String pattern]
    :   RUL IDENTIFIER
        {$value=$IDENTIFIER.text;$pattern=$IDENTIFIER.text;}
    |   {$value="*";$pattern="\\\\w*";}
    ;

alt_indicator_for_within
    :   COLON ai=INTLITERAL -> altFilterForWithin(altIndex={$ai.text})
    |   COLON NOT ai=INTLITERAL -> reverseAltFilterForWithin(altIndex={$ai.text})
    |   -> eptStr()
    ;

attr_indicator returns [String code]
    :   ATR IDENTIFIER {$code=$IDENTIFIER.text;}
    |   ATR {$code="*";}
    ;

token_filter
    :   lm=location_modifier LPAREN rn=rule_indicator ai=alt_indicator_for_within COLON tn=token_indicator RPAREN
        -> tokenFilter1(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, alt={$ai.st}, tokenName={$tn.code})
    |   lm=location_modifier LPAREN rn=rule_indicator ai=alt_indicator_for_within COLON NOT tn=token_indicator RPAREN
        -> tokenFilter2(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, alt={$ai.st}, tokenName={$tn.code})
    |   lm=location_modifier LPAREN NOT rn=rule_indicator ai=alt_indicator_for_within COLON tn=token_indicator RPAREN
        -> tokenFilter3(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={"\\\\w*"}, alt={$ai.st}, tokenName={$tn.code})
    |   lm=location_modifier LPAREN tn=token_indicator RPAREN
        -> tokenFilter1(locationModifier={$lm.code}, ruleNameValue={"*"}, alt={""}, tokenName={$tn.code})
    |   lm=location_modifier LPAREN NOT tn=token_indicator RPAREN
        -> tokenFilter2(locationModifier={$lm.code}, ruleNameValue={"*"}, alt={""}, tokenName={$tn.code})
    ;

token_indicator returns [String code]
    :   TKN IDENTIFIER {$code=$IDENTIFIER.text;}
    |   TKN {$code="*";}
    ;

nonterminel_filter
    :   lm=location_modifier LPAREN rn=rule_indicator ai=alt_indicator_for_within COLON srn=subrule_indicator RPAREN
        -> nonterminalFilter1(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, alt={$ai.st}, subruleName={$srn.code})
    |   lm=location_modifier LPAREN rn=rule_indicator ai=alt_indicator_for_within COLON NOT srn=subrule_indicator RPAREN
        -> nonterminalFilter2(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, alt={$ai.st}, subruleName={$srn.code})
    |   lm=location_modifier LPAREN NOT rn=rule_indicator ai=alt_indicator_for_within COLON srn=subrule_indicator RPAREN
        -> nonterminalFilter3(locationModifier={$lm.code}, ruleNameValue={$rn.value}, ruleNamePattern={"\\\\w*"}, alt={$ai.st}, subruleName={$srn.code})
    ;

subrule_indicator returns [String code]
    :   RUL IDENTIFIER {$code=$IDENTIFIER.text;}
    |   RUL {$code="*";}
    ;

branch_filter
    :   lm=location_modifier LPAREN rn=rule_indicator COLON bi=branch_indicator RPAREN
        -> branchFilter1(locationModifier={$lm.code}, ruleNameValue={$rn.value}, altIndex={$bi.code})
    |   lm=location_modifier LPAREN rn=rule_indicator COLON NOT bi=branch_indicator RPAREN
        -> branchFilter2(locationModifier={$lm.code}, ruleNameValue={$rn.value}, altIndex={$bi.code})
    |   lm=location_modifier LPAREN NOT rn=rule_indicator COLON bi=branch_indicator RPAREN
        -> branchFilter3(locationModifier={$lm.code}, ruleNameValue={$rn.value}, altIndex={$bi.code})
    |   lm=location_modifier LPAREN NOT rn=rule_indicator COLON NOT bi=branch_indicator RPAREN
        -> branchFilter4(locationModifier={$lm.code}, ruleNameValue={$rn.value}, altIndex={$bi.code})
    |   lm=location_modifier LPAREN rn=rule_indicator RPAREN
        -> branchFilter5(locationModifier={$lm.code}, ruleNameValue={$rn.value})
    |   lm=location_modifier LPAREN NOT rn=rule_indicator RPAREN
        -> branchFilter6(locationModifier={$lm.code}, ruleNameValue={$rn.value})
    ;

branch_indicator returns [String code]
    :   INTLITERAL {$code=$INTLITERAL.text;}
    |   {$code="*";}
    ;

range_filter
    :   wf=within_filter -> copyStr(f={$wf.st})
    |   cf=cflow_filter -> copyStr(f={$cf.st})
    |   cbf=cflowbelow_filter -> copyStr(f={$cbf.st})
    |   cpf=cflowPattern_filter -> copyStr(f={$cpf.st})
    ;

within_filter
    :   WTN LPAREN rn=rule_indicator ai=alt_indicator_for_within RPAREN
        -> withinFilter1(rangeModifier={"withincode"}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, alt={$ai.st})
    |   WTN LPAREN NOT rn=rule_indicator ai=alt_indicator_for_within RPAREN
        -> withinFilter2(rangeModifier={"withincode"}, ruleNameValue={$rn.value}, ruleNamePattern={"\\\\w*"}, alt={$ai.st})
    ;

cflow_filter
    :   CFW LPAREN rn=rule_indicator ai=alt_indicator_for_cflow RPAREN
        -> controlflowFilter1(rangeModifier={$CFW.text}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, ruleNameCflowPattern={$rn.pattern}, alt={$ai.st})
    |   CFW LPAREN NOT rn=rule_indicator ai=alt_indicator_for_cflow RPAREN
        -> controlflowFilter2(rangeModifier={$CFW.text}, ruleNameValue={$rn.value}, ruleNamePattern={"\\\\w*"}, ruleNameCflowPattern={""}, alt={$ai.st})
    ;

alt_indicator_for_cflow
    :   COLON ai=INTLITERAL -> altFilterForCflow(altIndex={$ai.text})
    |   COLON NOT ai=INTLITERAL -> reverseAltFilterForCflow(altIndex={$ai.text})
    |   -> eptStr()
    ;

cflowbelow_filter
    :   CFB LPAREN rn=rule_indicator ai=alt_indicator_for_cflowbelow RPAREN
        -> controlflowFilter1(rangeModifier={$CFB.text}, ruleNameValue={$rn.value}, ruleNamePattern={$rn.pattern}, ruleNameCflowPattern={$rn.pattern}, alt={$ai.st})
    |   CFB LPAREN NOT rn=rule_indicator ai=alt_indicator_for_cflowbelow RPAREN
        -> controlflowFilter2(rangeModifier={$CFB.text}, ruleNameValue={$rn.value}, ruleNamePattern={"\\\\w*"}, ruleNameCflowPattern={""}, alt={$ai.st})
    ;

alt_indicator_for_cflowbelow
    :   COLON ai=INTLITERAL -> altFilterForCflowbelow(altIndex={$ai.text})
    |   COLON NOT ai=INTLITERAL -> reverseAltFilterForCflowbelow(altIndex={$ai.text})
    |   -> eptStr()
    ;

cflowPattern_filter
    :   CFP LPAREN b=branch_element_list RPAREN
        -> cflowPatternFilter(pc={current_cflowpattern}, fc={$b.st})
    ;

branch_element_list
@init {
current_cflowpattern="\\\\[([\\\\w:,\\\\s]*)";
}
    :   b0=branch_element (',' b+=branch_element)*
        {current_cflowpattern+="\\\\]";}
        -> branchElementList(fb={$b0.st}, bl={$b})
    ;

branch_element returns [String btp]
    :   r=rule_indicator a=alt_indicator_for_cflowPattern
        {current_cflowpattern=current_cflowpattern+$r.pattern+":"+$a.altIndex+"([\\\\w:,\\\\s]*)";}
        -> branchElement(rulePattern={$r.value})
    ;

alt_indicator_for_cflowPattern returns [String altIndex]
    :   COLON INTLITERAL
        {$altIndex=$INTLITERAL.text;}
    |   {$altIndex="\\\\d*";}
    ;

advice_bound_filter
    : IDENTIFIER LPAREN RPAREN
      -> advBoundFlt(filterName={$IDENTIFIER.text})
    ;

block
    :   LBRACE
        b=blockStatement_list
        RBRACE
        -> blockTmpl(body={$b.st})
    ;

blockStatement_list
    :   (b+=blockStatement)*
        -> lineList(l={$b})
    ;

blockStatement
    :   l=localVariableDeclarationStatement
        -> copyStr(f={$l.st})
    |   c=classOrInterfaceDeclaration
        -> copyStr(f={$c.st})
    |   s=statement
        -> copyStr(f={$s.st})
    ;

statement
    :   b=block
        -> copyStr(f={$b.st})
    |   ('assert') expression (':' expression)? ';'
        -> copyStr(f={$text})
    |   'assert'  expression (':' expression)? ';'  
        -> copyStr(f={$text})
    |   'if' con=parExpression ib=statement eb=else_part
        -> conditionStatement(condition={$con.st}, ifBranch={$ib.st}, elseBranch={$eb.st})
    |   fs=forstatement
        -> copyStr(f={$fs.st})
    |   'while' con=parExpression lb=statement
        -> whileStatement(condition={$con.st}, loopBody={$lb.st})
    |   'do' lb=statement 'while' con=parExpression ';'
        -> doWhileStatement(condition={$con.st}, loopBody={$lb.st})
    |   ts=trystatement
        -> copyStr(f={$ts.st})
    |   'switch' con=parExpression '{' sb=switchBlockStatementGroups '}'
        -> switchStatement(condition={$con.st}, switchBody={$sb.st})
    |   'synchronized' p=parExpression b=block
        -> syncStatement(exp={$p.st}, syncBody={$b.st})
    |   'return' e=expression_oneornull_wrapper SEMI
        -> concatThree(e1={"return"}, e2={$e.st}, e3={$SEMI.text})
    |   'throw' e=expression SEMI
        -> concatThree(e1={"throw"}, e2={$e.st}, e3={$SEMI.text})
    |   'break' iow=identifier_oneornull_wrapper SEMI
        -> concatThree(e1={"break"}, e2={$iow.st}, e3={$SEMI.text})
    |   'continue' iow=identifier_oneornull_wrapper SEMI
        -> concatThree(e1={"continue"}, e2={$iow.st}, e3={$SEMI.text})
    |   e=expression SEMI     
        -> concatTwo(e1={$e.st}, e2={$SEMI.text})
    |   IDENTIFIER ':' p=statement
        -> colonStatement(a={$IDENTIFIER.text}, b={$p.st})
    |   ';'
        -> copyStr(f={$text})
    ;

else_part
    :   'else' eb=statement
        -> elsePart(ep={$eb.st})
    |   -> eptStr()
    ;

identifier_oneornull_wrapper
    :   IDENTIFIER
        -> copyStr(f={$text})
    |   -> eptStr()
    ;

/********************************************************************************************
                          Parser section
*********************************************************************************************/
    

compilationUnit 
    :   p=packageDeclaration_wrapper
        i=importDeclaration_list
        t=typeDeclaration_list
        -> concatFive(e1={$p.st}, e2={"\\n"}, e3={$i.st}, e4={"\\n"}, e5={$t.st})
    ;

annotations_wrapper
    :   a=annotations
        -> copyStr(f={$a.st})
    |   -> eptStr()
    ;

packageDeclaration_wrapper
    :   a=annotations_wrapper
        p=packageDeclaration
        -> concatThree(e1={$a.st},e2={"\\n"},e3={$p.st})
    |   -> eptStr()
    ;

importDeclaration_list
    :   (p+=importDeclaration)*
        -> lineList(l={$p})
    ;

importDeclaration  
    :   'import' 
        ('static'
        )?
        IDENTIFIER '.' '*'
        ';'       
    |   'import' 
        ('static'
        )?
        IDENTIFIER
        ('.' IDENTIFIER
        )+
        ('.' '*'
        )?
        ';'
    ;

qualifiedImportName 
    :   IDENTIFIER
        ('.' IDENTIFIER
        )*
    ;

typeDeclaration_list
    :   (t+=typeDeclaration)*
        -> lineList(l={$t})
    ;

typeDeclaration 
    :   c=classOrInterfaceDeclaration
        -> copyStr(f={$c.st})
    |   ';'
        -> copyStr(f={$text})
    ;

classOrInterfaceDeclaration 
    :   c=classDeclaration
        -> copyStr(f={$c.st})
    |   i=interfaceDeclaration
        -> copyStr(f={$i.st})
    ;
    
  
modifiers  
    :   (m+=modifiers_primary)*
        -> sepList(l={$m})
    ;

modifiers_primary
    :   a=annotation
        -> copyStr(f={$a.st})
    |   'public'
        -> copyStr(f={$text})
    |   'protected'
        -> copyStr(f={$text})
    |   'private'
        -> copyStr(f={$text})
    |   'static'
        -> copyStr(f={$text})
    |   'abstract'
        -> copyStr(f={$text})
    |   'final'
        -> copyStr(f={$text})
    |   'native'
        -> copyStr(f={$text})
    |   'synchronized'
        -> copyStr(f={$text})
    |   'transient'
        -> copyStr(f={$text})
    |   'volatile'
        -> copyStr(f={$text})
    |   'strictfp'
        -> copyStr(f={$text})
    ;

variableModifiers 
    :   (v+=variableModifier)*
        -> sepList(l={$v})
    ;
    
variableModifier
    :   'final'
        -> copyStr(f={$text})
    |   a=annotation
        -> copyStr(f={$a.st})
    ;

classDeclaration 
    :   n=normalClassDeclaration
        -> copyStr(f={$n.st})
    |   e=enumDeclaration
        -> copyStr(f={$e.st})
    ;

normalClassDeclaration 
    :   n=classNameDeclaration
        e=extends_type_wrapper
        i=implements_typeList_wrapper            
        b=classBody
        -> commonDeclarationTmpl(name={$n.st},suffix1={$e.st},suffix={$i.st},body={$b.st})
    ;

classNameDeclaration
    :   m=modifiers 'class' IDENTIFIER t=typeParameters_wrapper
        -> concatFour(e1={$m.st}, e2={" class "}, e3={$IDENTIFIER.text}, e4={$t.st})
    ;

typeParameters_wrapper
    :   t=typeParameters
        -> copyStr(f={$t.st})
    |   -> eptStr()
    ;

typeParameters 
    :   '<'
            t1=typeParameter
            t2=typeParameter_wrapper_list
        '>'
        -> concatFour(e1={"<"}, e2={$t1.st}, e3={$t2.st}, e4={">"})
    ;

typeParameter_wrapper_list
    :   (t+=typeParameter_wrapper)*
        -> concatList(l={$t})
    ;

typeParameter_wrapper
    :   ',' t=typeParameter
        -> concatTwo(e1={","},e2={$t.st})
    ;

typeParameter 
    :   IDENTIFIER
        e=extends_typeBound_wrapper
        -> concatTwo(e1={$IDENTIFIER.text},e2={$e.st})
    ;

extends_typeBound_wrapper
    :   'extends' t=typeBound
        -> concatTwo(e1={" extends "},e2={$t.st})
    |   -> eptStr()
    ;

typeBound 
    :   t1=type
        t2=amp_type_wrapper_list
        -> concatTwo(e1={$t1.st},e2={$t2.st})
    ;

amp_type_wrapper_list
    :   (a+=amp_type_wrapper)*
        -> concatList(l={$a})
    ;

amp_type_wrapper
    :   AMP t=type
        -> concatTwo(e1={$AMP.text},e2={$t.st})
    ;

extends_type_wrapper
    :   'extends' t=type
        -> concatTwo(e1={" extends "},e2={$t.st})
    |   -> eptStr()
    ;

implements_typeList_wrapper
    :   'implements' t=typeList
        -> concatTwo(e1={" implements "},e2={$t.st})
    |   -> eptStr()
    ;

enumDeclaration 
    :   en=enumNameDeclaration
        i=implements_typeList_wrapper
        eb=enumBody
        -> concatThree(e1={$en.st}, e2={$i.st}, e3={$eb.st})
    ;

enumNameDeclaration
    :   m=modifiers 
        ('enum'
        ) 
        IDENTIFIER
        -> concatThree(e1={$m.st}, e2={" enum "}, e3={$IDENTIFIER.text})
    ;

enumBody 
    :   LBRACE
        ec=enumConstants_oneornull_wrapper
        c=comma_oneornull_wrapper 
        eb=enumBodyDeclarations_oneornull_wrapper 
        RBRACE
        -> concatFive(e1={$LBRACE.text},e2={$ec.st},e3={$c.st},e4={$eb.st},e5={$RBRACE.text})
    ;

enumConstants_oneornull_wrapper
    :   e=enumConstants
        -> copyStr(f={$e.st})
    |   -> eptStr()
    ;

comma_oneornull_wrapper
    :   COMMA
        -> copyStr(f={$COMMA.text})
    |   -> eptStr()
    ;

enumBodyDeclarations_oneornull_wrapper
    :   e=enumBodyDeclarations
        -> copyStr(f={$e.st})
    |   -> eptStr()
    ;

enumConstants 
    :   e=enumConstant
        ewl=enumConstant_wrapper_list
        -> concatTwo(e1={$e.st},e2={$ewl.st})
    ;

enumConstant_wrapper_list
    :   (e+=enumConstant_wrapper)*
        -> concatList(l={$e})
    ;

enumConstant_wrapper
    :   ',' e=enumConstant
        -> concatTwo(e1={","},e2={$e.st})
    ;

/**
 * NOTE: here differs from the javac grammar, missing TypeArguments.
 * EnumeratorDeclaration = AnnotationsOpt [TypeArguments] IDENTIFIER [ Arguments ] [ "{" ClassBody "}" ]
 */
enumConstant 
    :   a1=annotations_wrapper
        IDENTIFIER
        a2=arguments_wrapper
        c=classBody_wrapper
        /* TODO: $GScope::name = names.empty. enum constant body is actually
        an anonymous class, where constructor isn't allowed, have to add this check*/
        -> concatFour(e1={$a1.st}, e2={" "+$IDENTIFIER.text+" "}, e3={$a2.st}, e4={$c.st})
    ;

enumBodyDeclarations 
    :   ';' c=classBodyDeclaration_list
        -> concatTwo(e1={";"},e2={$c.st})
    ;

classBodyDeclaration_list
    :   (c+=classBodyDeclaration)*
        -> concatList(l={$c})
    ;

interfaceDeclaration 
    :   n=normalInterfaceDeclaration
        -> copyStr(f={$n.st})
    |   a=annotationTypeDeclaration
        -> copyStr(f={$a.st})
    ;
    
normalInterfaceDeclaration 
    :   n=normalInterfaceNameDeclaration
        t=typeParameters_wrapper
        e=extends_typeList_wrapper
        i=interfaceBody
        -> commonDeclarationTmpl(name={$n.st},suffix1={$t.st},suffix={$e.st},body={$i.st})
    ;

normalInterfaceNameDeclaration
    :   m=modifiers 'interface' IDENTIFIER
        -> concatThree(e1={$m.st}, e2={" interface "}, e3={$IDENTIFIER.text})
    ;

extends_typeList_wrapper
    :   'extends' t=typeList
        -> concatTwo(e1={" extends "},e2={$t.st})
    |   -> eptStr()
    ;

typeList 
    :   t=type c=comma_type_wrapper_list
        -> concatTwo(e1={$t.st},e2={$c.st})
    ;

comma_type_wrapper_list
    :   (c+=comma_type_wrapper)*
        -> concatList(l={$c})
    ;

comma_type_wrapper
    :   COMMA t=type
        -> concatTwo(e1={$COMMA.text},e2={$t.st})
    ;

classBody 
    :   LBRACE 
        c=classBodyDeclaration_list 
        RBRACE
        -> concatThree(e1={$LBRACE.text}, e2={$c.st}, e3={$RBRACE.text})
    ;

interfaceBody 
    :   LBRACE 
        i=interfaceBodyDeclaration_list
        RBRACE
        -> concatThree(e1={$LBRACE.text}, e2={$i.st}, e3={$RBRACE.text})
    ;

interfaceBodyDeclaration_list
    :   (p+=interfaceBodyDeclaration)* 
        -> concatList(l={$p})
    ;

classBodyDeclaration 
    :   ';'
        -> copyStr(f={$text})
    |   s=static_oneornull_wrapper 
        b=block
        -> concatTwo(e1={$s.st},e2={$b.st})
    |   m=memberDecl
        -> copyStr(f={$m.st})
    ;

static_oneornull_wrapper
    :   'static'
        -> copyStr(f={$text})
    |   -> eptStr()
    ;

memberDecl 
    :   fd=fieldDeclaration
        -> copyStr(f={$fd.st})
    |   m=methodDeclaration
        -> copyStr(f={$m.st})
    |   c=classDeclaration
        -> copyStr(f={$c.st})
    |   i=interfaceDeclaration
        -> copyStr(f={$i.st})
    ;


methodDeclaration 
    :   m=methodNameShortDeclaration
        f=formalParameters
        t=throws_qualifiedNameList_wrapper
        c=cons_block_wrapper
        -> concatFour(e1={$m.st}, e2={$f.st}, e3={$t.st}, e4={$c.st})
    |   m=methodLongDeclarationPart
        b=block_wrapper
        -> concatTwo(e1={$m.st},e2={$b.st})
    ;

methodNameShortDeclaration
    :   m=modifiers
        t=typeParameters_wrapper
        IDENTIFIER
        -> concatFour(e1={$m.st}, e2={" "}, e3={$t.st}, e4={" "+$IDENTIFIER.text})
    ;

methodNameLongDeclaration
    :   m=modifiers
        t1=typeParameters_wrapper
        t2=type_indicator
        IDENTIFIER
        -> concatSix(e1={$m.st}, e2={" "}, e3={$t1.st}, e4={" "}, e5={$t2.st}, e6={" "+$IDENTIFIER.text})
    ;

methodLongDeclarationPart
    :   m=methodNameLongDeclaration
        f=formalParameters
        s=square_bracket_pair_list
        t=throws_qualifiedNameList_wrapper
        -> concatFour(e1={$m.st}, e2={$f.st}, e3={$s.st}, e4={$t.st})
    ;

cons_block_wrapper
    :   LBRACE 
        e=explicitConstructorInvocation_wrapper
        b=blockStatement_list
        RBRACE
        -> concatFour(e1={$LBRACE.text}, e2={$e.st}, e3={$b.st}, e4={$RBRACE.text})
    ;

block_wrapper
    :   b=block
        -> copyStr(f={$b.st})
    |   ';'
        -> copyStr(f={$text})
    ;

throws_qualifiedNameList_wrapper
    :   'throws' q=qualifiedNameList
        -> concatTwo(e1={" throws "},e2={$q.st})
    |   -> eptStr()
    ;

explicitConstructorInvocation_wrapper
    :   e=explicitConstructorInvocation
        -> copyStr(f={$e.st})
    |   -> eptStr()
    ;

type_indicator
    :   t=type
        -> copyStr(f={$t.st})
    |   'void'
        -> copyStr(f={$text})
    ;

fieldDeclaration 
    :   m=modifiers
        t=type
        v1=variableDeclarator
        v2=variableDeclarator_wrapper_list
        ';'
        -> fieldDeclarationTmpl(mod={$m.st},typ={$t.st},vd={$v1.st},vdl={$v2.st})
    ;

variableDeclarator_wrapper_list
    :   (v+=variableDeclarator_wrapper)*
        -> concatList(l={$v})
    ;

variableDeclarator_wrapper
    :   ',' v=variableDeclarator
        -> concatTwo(e1={";"},e2={$v.st})
    ;

variableDeclarator 
    :   IDENTIFIER
        s=square_bracket_pair_list
        v=variableInitializer_wrapper
        -> concatThree(e1={$IDENTIFIER.text}, e2={$s.st}, e3={$v.st})
    ;

variableInitializer_wrapper
    :   '=' v=variableInitializer
        -> concatTwo(e1={"="},e2={$v.st})
    |   -> eptStr()
    ;

/**
 *TODO: add predicates
 */
interfaceBodyDeclaration 
    :   i=interfaceFieldDeclaration
        -> copyStr(f={$i.st})
    |   i=interfaceMethodDeclaration
        -> copyStr(f={$i.st})
    |   i=interfaceDeclaration
        -> copyStr(f={$i.st})
    |   c=classDeclaration
        -> copyStr(f={$c.st})
    |   ';'
        -> copyStr(f={$text})
    ;

interfaceMethodDeclaration 
    :   m=methodLongDeclarationPart ';'
        -> concatTwo(e1={$m.st},e2={";"})
    ;

/**
 * NOTE, should not use variableDeclarator here, as it doesn't necessary require
 * an initializer, while an interface field does, or judge by the returned value.
 * But this gives better diagnostic message, or antlr won't predict this rule.
 */
interfaceFieldDeclaration 
    :   m=modifiers
        t=type
        v1=variableDeclarator
        v2=variableDeclarator_wrapper_list
        ';'
        -> fieldDeclarationTmpl(mod={$m.st},typ={$t.st},vd={$v1.st},vdl={$v2.st})
    ;

type 
    :   c=classOrInterfaceType
        s=square_bracket_pair_list
        -> concatTwo(e1={$c.st}, e2={$s.st})
    |   primitiveType
        s=square_bracket_pair_list
        -> concatTwo(e1={$primitiveType.text}, e2={$s.st})
    ;


classOrInterfaceType 
    :   IDENTIFIER
        t=typeArgumentsWrapper
        c=classOrInterfaceType_mainBody_wrapper
        -> concatThree(e1={$IDENTIFIER.text}, e2={$t.st}, e3={$c.st})
    ;

classOrInterfaceType_mainBody_wrapper
    :   (c+=classOrInterfaceType_mainBody)*
        -> concatList(l={$c})
    ;

classOrInterfaceType_mainBody
    :   DOT IDENTIFIER t=typeArgumentsWrapper
        -> concatThree(e1={$DOT.text}, e2={$IDENTIFIER.text}, e3={$t.st})
    ;

typeArgumentsWrapper
    :   t=typeArguments
        -> copyStr(f={$t.st})
    |   -> eptStr()
    ;

primitiveType
    :   'boolean'
    |   'char'
    |   'byte'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    |   'String'
    ;

typeArguments 
    :   '<' ta=typeArgument
        tal=typeArgument_list
        '>'
        -> concatFour(e1={"<"}, e2={$ta.st}, e3={$tal.st}, e4={">"})
    ;

typeArgument_list
    :   (t+=typeArgument_primary)*
        -> concatList(l={$t})
    ;

typeArgument_primary
    :   COMMA t=typeArgument
        -> concatTwo(e1={$COMMA.text}, e2={$t.st})
    ;

typeArgument 
    :   t=type
        -> copyStr(f={$t.st})
    |   '?' r=relation_indicator_wrapper
        -> concatTwo(e1={"?"}, e2={$r.st})
    ;

relation_indicator_wrapper
    :   r=relation_indicator t=type
        -> concatTwo(e1={$r.text}, e2={$t.st})
    |   -> eptStr()
    ;

relation_indicator
    :   'extends'
    |   'super'
    ;

qualifiedNameList 
    :   q1=qualifiedName
        q2=qualifiedName_wrapper_list
        -> concatTwo(e1={$q1.text}, e2={$q2.st})
    ;

qualifiedName_wrapper_list
    :   (q+=qualifiedName_wrapper)*
        -> concatList(l={$q})
    ;

qualifiedName_wrapper
    :   ',' q=qualifiedName
        -> concatTwo(e1={","}, e2={$q.text})
    ;

formalParameters 
    :   '('
        f=formalParameterDecls_wrapper 
        ')'
        -> concatThree(e1={"("}, e2={$f.st}, e3={")"})
    ;

formalParameterDecls_wrapper
    :   fpd=formalParameterDecls
        -> copyStr(f={$fpd.st})
    |   -> eptStr()
    ;

formalParameterDecls 
    :   ellipsisParameterDecl
        -> copyStr(f={$e.st})
    |   n=normalParameterDecl
        c=comma_normalParameterDecl_wrapper_list
        -> concatTwo(e1={$n.st}, e2={$c.st})
    |   n=normalParameterDecl_COMMA_wrapper_plus_list
        e=ellipsisParameterDecl
        -> concatTwo(e1={$n.st}, e2={$e.st})
    ;

comma_normalParameterDecl_wrapper_list
    :   (c+=comma_normalParameterDecl_wrapper)*
        -> concatList(l={$c})
    ;

comma_normalParameterDecl_wrapper
    :   ',' n=normalParameterDecl
        -> concatTwo(e1={","}, e2={$n.st})
    ;

normalParameterDecl_COMMA_wrapper_plus_list
    :   (n+=normalParameterDecl_COMMA_wrapper)+
        -> concatList(l={$n})
    ;

normalParameterDecl_COMMA_wrapper
    :   n=normalParameterDecl ','
        -> concatTwo(e1={$n.st}, e2={","})
    |   -> eptStr()
    ;

normalParameterDecl 
    :   v=variableModifiers
        t=type IDENTIFIER
        s=square_bracket_pair_list
        -> concatFour(e1={$v.st+" "}, e2={$t.st+" "}, e3={$IDENTIFIER.text}, e4={$s.st})
    ;

ellipsisParameterDecl 
    :   v=variableModifiers
        t=type  '...'
        IDENTIFIER
        -> concatFour(e1={$v.st}, e2={$t.st}, e3={"..."}, e4={$IDENTIFIER.text})
    ;

explicitConstructorInvocation 
    :   n=nonWildcardTypeArguments_wrapper
        c=cons_caller_indicator
        a=arguments ';'
        -> concatFour(e1={$n.st}, e2={$c.text}, e3={$a.st}, e4={";"})
    |   p=primary DOT
        n=nonWildcardTypeArguments_wrapper
        'super' a=arguments SEMI
        -> concatSix(e1={$p.st}, e2={$DOT.text}, e3={$n.st}, e4={"super"}, e5={$a.st}, e6={$SEMI.text})
    ;

cons_caller_indicator
    :   'this'
    |   'super'
    ;

qualifiedName 
    :   IDENTIFIER
        ('.' IDENTIFIER
        )*
    ;

annotations 
    :   (a+=annotation)+
        -> concatList(l={$a})
    ;

/**
 *  Using an annotation. 
 * '@' is flaged in modifier
 */
annotation 
    :   RUL q=qualifiedName
        p=parentheses_elementValuePairsOrelementValue_wrapper_wrapper
        -> concatThree(e1={$RUL.text}, e2={$q.text}, e3={$p.st})
    ;

parentheses_elementValuePairsOrelementValue_wrapper_wrapper
    :   p=parentheses_elementValuePairsOrelementValue_wrapper
        -> copyStr(f={$p.st})
    |   -> eptStr()
    ;

parentheses_elementValuePairsOrelementValue_wrapper
    :   '(' e=elementValuePairsOrelementValue_wrapper ')'
        -> concatThree(e1={"("}, e2={$e.st}, e3={")"})
    ;

elementValuePairsOrelementValue_wrapper
    :   e=elementValuePairsOrelementValue
        -> copyStr(f={$e.st})
    |   -> eptStr()
    ;

elementValuePairsOrelementValue
    :   e=elementValuePairs
        -> copyStr(f={$e.st})
    |   e=elementValue
        -> copyStr(f={$e.st})
    ;

elementValuePairs 
    :   ep=elementValuePair
        epl=elementValuePair_wrapper_list
        -> concatTwo(e1={$ep.st}, e2={$epl.st})
    ;

elementValuePair_wrapper_list
    :   (e+=elementValuePair_wrapper)*
        -> concatList(l={$e})
    ;

elementValuePair_wrapper
    :   ',' e=elementValuePair
        -> concatTwo(e1={","}, e2={$e.st})
    ;

elementValuePair 
    :   IDENTIFIER '=' e=elementValue
        -> concatThree(e1={$IDENTIFIER.text}, e2={"="}, e3={$e.st})
    ;

elementValue 
    :   c=conditionalExpression
        -> copyStr(f={$c.st})
    |   a=annotation
        -> copyStr(f={$a.st})
    |   e=elementValueArrayInitializer
        -> copyStr(f={$e.st})
    ;

elementValueArrayInitializer 
    :   LBRACE
        e=elementValue_wrapper_list_wrapper
        c=comma_oneornull_wrapper
        RBRACE
        -> concatFour(e1={$LBRACE.text}, e2={$e.st}, e3={$c.st}, e4={$RBRACE.text})
    ;

elementValue_wrapper_list_wrapper
    :   e=elementValue
        ewl=elementValue_wrapper_list
        -> concatTwo(e1={$e.st}, e2={$ewl.st})
    ;

elementValue_wrapper_list
    :   (e+=elementValue_wrapper)*
        -> concatList(l={$e})
    ;

elementValue_wrapper
    :   ',' e=elementValue
        -> concatTwo(e1={","}, e2={$e.st})
    ;

/**
 * Annotation declaration.
 */
annotationTypeDeclaration 
    :   m=modifiers '@'
        'interface'
        IDENTIFIER
        a=annotationTypeBody
        -> concatFour(e1={$m.st}, e2={"@interface"}, e3={$IDENTIFIER.text}, e4={$a.st})
    ;

annotationTypeBody 
    :   LBRACE 
        a=annotationTypeElementDeclaration_list
        RBRACE
        -> concatThree(e1={$LBRACE.text}, e2={$a.st}, e3={$RBRACE.text})
    ;

annotationTypeElementDeclaration_list
    :   (a+=annotationTypeElementDeclaration)*
        -> concatList(l={$a})
    ;

/**
 * NOTE: here use interfaceFieldDeclaration for field declared inside annotation. they are sytactically the same.
 */
annotationTypeElementDeclaration 
    :   a=annotationMethodDeclaration
        -> copyStr(f={$a.st})
    |   i=interfaceFieldDeclaration
        -> copyStr(f={$i.st})
    |   n=normalClassDeclaration
        -> copyStr(f={$n.st})
    |   n=normalInterfaceDeclaration
        -> copyStr(f={$n.st})
    |   e=enumDeclaration
        -> copyStr(f={$e.st})
    |   a=annotationTypeDeclaration
        -> copyStr(f={$a.st})
    |   ';'
        -> copyStr(f={$text})
    ;

annotationMethodDeclaration 
    :   a=annotationMethodNameDeclaration
        '(' ')' d=default_elementValue_wrapper SEMI
        -> concatFour(e1={$a.st}, e2={"()"}, e3={$d.st}, e4={$SEMI.text})
        ;

annotationMethodNameDeclaration
    :   m=modifiers
        t=type
        IDENTIFIER
        -> concatFive(e1={$m.st}, e2={" "}, e3={$t.st}, e4={" "}, e5={$IDENTIFIER.text})
    ;

default_elementValue_wrapper
    :   'default' e=elementValue
        -> concatTwo(e1={"default"}, e2={$e.st})
    |   -> eptStr()
    ;

/*
staticBlock returns [JCBlock tree]
        @init {
            ListBuffer<JCStatement> stats = new ListBuffer<JCStatement>();
            int pos = ((AntlrJavacToken) $start).getStartIndex();
        }
        @after {
            $tree = T.at(pos).Block(Flags.STATIC, stats.toList());
            pu.storeEnd($tree, $stop);
            // construct a dummy static modifiers for end position
            pu.storeEnd(T.at(pos).Modifiers(Flags.STATIC,  com.sun.tools.javac.util.List.<JCAnnotation>nil()),$st);
        }
    :   st_1='static' '{' 
        (blockStatement
            {
                if ($blockStatement.tree == null) {
                    stats.appendList($blockStatement.list);
                } else {
                    stats.append($blockStatement.tree);
                }
            }
        )* '}'
    ;
*/

localVariableDeclarationStatement 
    :   l=localVariableDeclaration SEMI
        -> concatTwo(e1={$l.st},e2={$SEMI.text})
    ;

localVariableDeclaration 
    :   v1=variableModifiers t=type
        v2=variableDeclarator
        v3=variableDeclarator_wrapper_list
        -> concatSix(e1={$v1.st}, e2={" "}, e3={$t.st}, e4={" "}, e5={$v2.st}, e6={$v3.st})
    ;

switchBlockStatementGroups 
    :   (s+=switchBlockStatementGroup)*
        -> concatList(l={$s})
    ;

switchBlockStatementGroup 
    :   s=switchLabel
        b=blockStatement_list
        -> concatTwo(e1={$s.st},e2={$b.st})
    ;

switchLabel 
    :   'case' e=expression ':'
        -> concatThree(e1={"case"}, e2={$e.st}, e3={":"})
    |   'default' ':'
        -> copyStr(f={$text})
    ;


trystatement 
    :   'try' b=block c=catches_part
        -> concatThree(e1={"try"}, e2={$b.st}, e3={$c.st})
    ;

catches_part
    :   c=catches 'finally' b=block
        -> concatThree(e1={$c.st}, e2={"finally"}, e3={$b.st})
    |   c=catches
        -> copyStr(f={$c.st})
    |   'finally' b=block
        -> concatTwo(e1={"finally"},e2={$b.st})
    ;

catches 
    :   c1=catchClause
        c2=catchClause_list
        -> concatTwo(e1={$c1.st},e2={$c2.st})
    ;

catchClause_list
    :   (c+=catchClause)*
        -> concatList(l={$c})
    ;

catchClause 
    :   'catch' '(' f=formalParameter
        ')' b=block 
        -> concatFour(e1={"catch("}, e2={$f.st}, e3={")"}, e4={$b.st})
    ;

formalParameter 
    :   v=variableModifiers t=type IDENTIFIER
        s=square_bracket_pair_list
        -> concatFour(e1={$v.st}, e2={$t.st}, e3={$IDENTIFIER.text}, e4={$s.st})
    ;

forstatement 
    :   'for' '(' f=foreach_condition ')' s=statement
        -> concatFour(e1={"for("}, e2={$f.st}, e3={")"}, e4={$s.st})
    |   'for' '(' f=for_condition ')' s=statement
        -> concatFour(e1={"for("}, e2={$f.st}, e3={")"}, e4={$s.st})
    ;

foreach_condition
    :   v=variableModifiers t=type IDENTIFIER ':' 
        e=expression
        -> concatFive(e1={$v.st}, e2={$t.st}, e3={$IDENTIFIER.text}, e4={":"}, e5={$e.st})
    ;

for_condition
    :   f=forInit_wrapper ';' 
        eow=expression_oneornull_wrapper ';' 
        ew=expressionList_wrapper
        -> concatFive(e1={$f.st}, e2={";"}, e3={$eow.st}, e4={";"}, e5={$ew.st})
    ;

forInit_wrapper
    :   p=forInit
        -> copyStr(f={$p.st})
    |   -> eptStr()
    ;

forInit 
    :   l=localVariableDeclaration
        -> copyStr(f={$l.st})
    |   e=expressionList
        -> copyStr(f={$e.st})
    ;

expression_oneornull_wrapper
    :   e=expression
        -> copyStr(f={$e.st})
    |   -> eptStr()
    ;

parExpression 
    :   '(' e=expression ')'
        -> concatThree(e1={"("}, e2={$e.st}, e3={")"})
    ;

expressionList 
    :   e=expression
        ewl=comma_expression_wrapper_list
        -> concatTwo(e1={$e.st},e2={$ewl.st})
    ;

comma_expression_wrapper_list
    :   (ew+=comma_expression_wrapper)*
        -> concatList(l={$ew})
    ;

comma_expression_wrapper
    :   ',' e=expression
        -> concatTwo(e1={","},e2={$e.st})
    ;

expression 
    :   c=conditionalExpression
        a=assignmentOperator_expression
        -> concatTwo(e1={$c.st},e2={$a.st})
    ;

assignmentOperator_expression
    :   a=assignmentOperator e=expression
        -> concatTwo(e1={$a.text},e2={$e.st})
    |   -> eptStr()
    ;

assignmentOperator 
    :   '='
    |   '+='
    |   '-='
    |   '*='
    |   '/='
    |   '&='
    |   '|='
    |   '^='
    |   '%='
    |    '<' '<' '='
    |    '>' '>' '>' '='
    |    '>' '>' '='
    ;

conditionalExpression 
    :   c1=conditionalOrExpression
        c2=conditionalExpression_f
        -> concatTwo(e1={$c1.st},e2={$c2.st})
    ;

conditionalExpression_f
    :   '?' e=expression ':' c=conditionalExpression
        -> concatFour(e1={"?"},e2={$e.st},e3={":"},e4={$c.st})
    |   -> eptStr()
    ;

conditionalOrExpression 
    :   c1=conditionalAndExpression
        c2=conditionalOrExpression_f
        -> concatTwo(e1={$c1.st},e2={$c2.st})
    ;

conditionalOrExpression_f
    :   (c+=conditionalAndExpression_wrapper)*
        -> concatList(l={$c})
    ;

conditionalAndExpression_wrapper
    :   '||' c=conditionalAndExpression
        -> concatTwo(e1={"||"},e2={$c.st})
    ;

conditionalAndExpression 
    :   i=inclusiveOrExpression
        c=conditionalAndExpression_f
        -> concatTwo(e1={$i.st},e2={$c.st})
    ;

conditionalAndExpression_f
    :   (i+=inclusiveOrExpression_wrapper)*
        -> concatList(l={$i})
    ;

inclusiveOrExpression_wrapper
    :   '&&' i=inclusiveOrExpression
        -> concatTwo(e1={"&&"},e2={$i.st})
    ;

inclusiveOrExpression 
    :   e=exclusiveOrExpression
        i=inclusiveOrExpression_f
        -> concatTwo(e1={$e.st},e2={$i.st})
    ;

inclusiveOrExpression_f
    :   (e+=exclusiveOrExpression_wrapper)*
        -> concatList(l={$e})
    ;

exclusiveOrExpression_wrapper
    :   '|' e=exclusiveOrExpression
        -> concatTwo(e1={"|"},e2={$e.st})
    ;

exclusiveOrExpression 
    :   a=andExpression
        e=exclusiveOrExpression_f
        -> concatTwo(e1={$a.st},e2={$e.st})
    ;

exclusiveOrExpression_f
    :   (a+=andExpression_wrapper)*
        -> concatList(l={$a})
    ;

andExpression_wrapper
    :   '^' a=andExpression
        -> concatTwo(e1={"^"},e2={$a.st})
    ;

andExpression 
    :   e=equalityExpression
        a=andExpression_f
        -> concatTwo(e1={$e.st},e2={$a.st})
    ;

andExpression_f
    :   (e+=equalityExpression_wrapper)*
        -> concatList(l={$e})
    ;

equalityExpression_wrapper
    :   '&' e=equalityExpression
        -> concatTwo(e1={"&"},e2={$e.st})
    ;

equalityExpression 
    :   i1=instanceOfExpression
        i2=equalityExpression_f
        -> concatTwo(e1={$i1.st},e2={$i2.st})
    ;

equalityExpression_f
    :   (i+=instanceOfExpression_wrapper)*
        -> concatList(l={$i})
    ;

instanceOfExpression_wrapper
    :   c=compare_operator
        i=instanceOfExpression
        -> concatTwo(e1={$c.text},e2={$i.st})
    ;

compare_operator
    :   '=='
    |   '!='
    ;

instanceOfExpression 
    :   r=relationalExpression
        i=instanceOfExpression_f
        -> concatTwo(e1={$r.st},e2={$i.st})
    ;

instanceOfExpression_f
    :   'instanceof' t=type
        -> concatTwo(e1={"instanceof"},e2={$t.st})
    |   -> eptStr()
    ;

relationalExpression 
    :   s=shiftExpression
        r=relationalExpression_f
        -> concatTwo(e1={$s.st},e2={$r.st})
    ;

relationalExpression_f
    :   (s+=shiftExpression_wrapper)*
        -> concatList(l={$s})
    ;

shiftExpression_wrapper
    :   r=relationalOp
        s=shiftExpression
        -> concatTwo(e1={$r.text},e2={$s.st})
    ;

relationalOp 
    :   '<' '='
    |   '>' '='
    |   '<'
    |   '>'
    ;

shiftExpression 
    :   a=additiveExpression
        s=shiftExpression_f
        -> concatTwo(e1={$a.st},e2={$s.st})
    ;

shiftExpression_f
    :   (a+=additiveExpression_wrapper)*
        -> concatList(l={$a})
    ;

additiveExpression_wrapper
    :   s=shiftOp a=additiveExpression
        -> concatTwo(e1={$s.text},e2={$a.st})
    ;

shiftOp 
    :    '<' '<'
    |    '>' '>' '>'
    |    '>' '>'
    ;


additiveExpression 
    :   m=multiplicativeExpression
        a=additiveExpression_f
        -> concatTwo(e1={$m.st},e2={$a.st})
    ;

additiveExpression_f
    :   (m+=multiplicativeExpression_wrapper)*
        -> concatList(l={$m})
    ;

multiplicativeExpression_wrapper
    :   a=additive_operator
        m=multiplicativeExpression
        -> concatTwo(e1={$a.text},e2={$m.st})
    ;

additive_operator
    :   '+'
    |   '-'
    ;
            
multiplicativeExpression 
    :   u=unaryExpression
        m=multiplicativeExpression_f
        -> concatTwo(e1={$u.st},e2={$m.st})
    ;

multiplicativeExpression_f
    :   (u+=unaryExpression_wrapper)*
        -> concatList(l={$u})
    ;

unaryExpression_wrapper
    :   uo=unary_operator
        ue=unaryExpression
        -> concatTwo(e1={$uo.text},e2={$ue.st})
    ;

unary_operator
    :   '*'
    |   '/'
    |   '%'
    ;


/**
 * NOTE: for '+' and '-', if the next token is int or long interal, then it's not a unary expression.
 *       it's a literal with signed value. INTLTERAL AND LONG LITERAL are added here for this.
 */
unaryExpression 
    :   '+' u=unaryExpression
        -> concatTwo(e1={"+"},e2={$u.st})
    |   '-' u=unaryExpression
        -> concatTwo(e1={"-"},e2={$u.st})
    |   '++' u=unaryExpression
        -> concatTwo(e1={"++"},e2={$u.st})
    |   '--' u=unaryExpression
        -> concatTwo(e1={"--"},e2={$u.st})
    |   u=unaryExpressionNotPlusMinus
        -> copyStr(f={$u.st})
    ;

unaryExpressionNotPlusMinus 
    :   '~' u=unaryExpression
        -> concatTwo(e1={"~"},e2={$u.st})
    |   '!' u=unaryExpression
        -> concatTwo(e1={"!"},e2={$u.st})
    |   c=castExpression
        -> copyStr(f={$c.st})
    |   p=primary
        sl=selector_list
        sow=self_operator_wrapper
        -> concatThree(e1={$p.st}, e2={$sl.st}, e3={$sow.st})
    ;

selector_list
    :   (s+=selector)*
        -> concatList(l={$s})
    ;

self_operator_wrapper
    :   s=self_operator
        -> copyStr(f={$s.text})
    |   -> eptStr()
    ;

self_operator
    :   '++'
    |   '--'
    ;

castExpression 
    :   '(' p=primitiveType ')' u=unaryExpression
        -> concatFour(e1={"("}, e2={$p.text}, e3={")"}, e4={$u.st})
    |   '(' t=type ')' u=unaryExpressionNotPlusMinus
        -> concatFour(e1={"("}, e2={$t.st}, e3={")"}, e4={$u.st})
    ;

/**
 * have to use scope here, parameter passing isn't well supported in antlr.
 */
primary 
    :   op=oba_primary
        -> copyStr(f={$op.st})
    |   p=parExpression            
        -> copyStr(f={$p.st})
    |   'this'
        il=dot_identifier_wrapper_list
        iw=identifierSuffix_wrapper
        -> concatThree(e1={"this"}, e2={$il.st}, e3={$iw.st})
    |   IDENTIFIER
        il=dot_identifier_wrapper_list
        iw=identifierSuffix_wrapper
        -> concatThree(e1={$IDENTIFIER.text}, e2={$il.st}, e3={$iw.st})
    |   'super'
        s=superSuffix
        -> concatTwo(e1={"super"},e2={$s.st})
    |   literal
        -> copyStr(f={$text})
    |   c=creator
        -> copyStr(f={$c.st})
    |   primitiveType
        ('[' ']'
        )*
        '.' 'class'
        -> copyStr(f={$text})
    |   'void' '.' 'class'
        -> copyStr(f={$text})
    ;

identifierSuffix_wrapper
    :   i=identifierSuffix
        -> copyStr(f={$i.st})
    |   -> eptStr()
    ;

dot_identifier_wrapper_list
    :   (p+=dot_identifier_wrapper)*
        -> concatList(l={$p})
    ;

dot_identifier_wrapper
    :   '.' IDENTIFIER
        -> copyStr(f={$text})
    ;

superSuffix  
    :   a=arguments
        -> copyStr(f={$a.st})
    |   '.' t=typeArguments_wrapper
        IDENTIFIER
        a=arguments_wrapper
        -> concatFour(e1={"."}, e2={$t.st}, e3={$IDENTIFIER.text}, e4={$a.st})
    ;

typeArguments_wrapper
    :   t=typeArguments
        -> copyStr(f={$t.st})
    |   -> eptStr()
    ;

arguments_wrapper
    :   a=arguments
        -> copyStr(f={$a.st})
    |   -> eptStr()
    ;

identifierSuffix 
    :   s=square_bracket_pair_plus_list
        '.' 'class'
        -> concatThree(e1={$s.st}, e2={"."}, e3={"class"})
    |   s=square_bracket_wrapped_expression_plus_list
        -> copyStr(f={$s.st})
    |   a=arguments
        -> copyStr(f={$a.st})
    |   '.' 'class'
        -> copyStr(f={$text})
    |   '.' n=nonWildcardTypeArguments IDENTIFIER a=arguments
        -> concatFour(e1={"."}, e2={$n.st}, e3={$IDENTIFIER.text}, e4={$a.st})
    |   '.' 'this'
        -> copyStr(f={$text})
    |   '.' 'super' a=arguments
        -> concatThree(e1={"."}, e2={"super"}, e3={$a.st})
    |   i=innerCreator
        -> copyStr(f={$i.st})
    ;

square_bracket_pair_plus_list
    :   (p+=square_bracket_pair)+
        -> concatList(l={$p})
    ;

square_bracket_wrapped_expression_plus_list
    :   (p+=square_bracket_wrapped_expression)+
        -> concatList(l={$p})
    ;

selector  
    :   '.' IDENTIFIER
        a=arguments_wrapper
        -> concatThree(e1={"."},e2={$IDENTIFIER.text}, e3={$a.st})
    |   '.' 'this'
        -> copyStr(f={$text})
    |   '.' 'super'
        s=superSuffix
        -> concatThree(e1={"."}, e2={"super"}, e3={$s.st})
    |   i=innerCreator
        -> copyStr(f={$i.st})
    |   s=square_bracket_wrapped_expression
        -> copyStr(f={$s.st})
    ;

creator 
    :   'new' n=nonWildcardTypeArguments c1=classOrInterfaceType c2=classCreatorRest
        -> concatFour(e1={"new "}, e2={$n.st}, e3={$c1.st}, e4={$c2.st})
    |   'new' c1=classOrInterfaceType c2=classCreatorRest
        -> concatThree(e1={"new "}, e2={$c1.st}, e3={$c2.st})
    |   a=arrayCreator
        -> copyStr(f={$a.st})
    ;

arrayCreator 
    :   'new' a=createdName
        b=square_bracket_pair
        c=square_bracket_pair_list
        d=arrayInitializer
        -> arrayCreatorTmpl(name={$a.st}, e1={$b.st}, e2={$c.st}, e3={$d.st})
    |   'new' a=createdName
        b=square_bracket_wrapped_expression
        c=square_bracket_wrapped_expression_list
        d=square_bracket_pair_list
        -> arrayCreatorTmpl(name={$a.st}, e1={$b.st}, e2={$c.st}, e3={$d.st})
    ;

square_bracket_wrapped_expression_list
    :   (p+=square_bracket_wrapped_expression)*
        -> concatList(l={$p})
    ;

square_bracket_wrapped_expression
    :   '[' e=expression ']'
        -> squareBracketWrappedExpression(exp={$e.st})
    ;

square_bracket_pair_list
    :   (p+=square_bracket_pair)*
        -> concatList(l={$p})
    ;

square_bracket_pair
    :   '[]' -> copyStr(f={"[]"})
    ;

arrayInitializer 
    :   '{' 
            aim=arrayInitializer_mainBody 
            ais=arrayInitializer_suffix
        '}'             //Yang's fix, position change.
        -> concatFour(e1={"{"}, e2={$aim.st}, e3={$ais.st}, e4={"}"})
    ;

arrayInitializer_mainBody
    :   v1=variableInitializer v2=variable_initializer_list
        -> concatTwo(e1={$v1.st}, e2={$v2.st})
    |   -> eptStr()
    ;

arrayInitializer_suffix
    :   COMMA
        -> copyStr(f={$COMMA.text})
    |   -> eptStr()
    ;

variableInitializer 
    :   a=arrayInitializer
        -> copyStr(f={$a.st})
    |   e=expression
        -> copyStr(f={$e.st})
    ;

variable_initializer_list
    :   (v+=variable_initializer)*
        -> concatList(l={$v})
    ;

variable_initializer
    :   COMMA v=variableInitializer
        -> concatTwo(e1={$COMMA.text}, e2={$v.st})
    ;

createdName 
    :   c=classOrInterfaceType
        -> copyStr(f={$c.st})
    |   primitiveType
        -> copyStr(f={$primitiveType.text})
    ;

innerCreator  
    :   '.' 'new'
        n=nonWildcardTypeArguments_wrapper
        IDENTIFIER
        t=typeArguments_wrapper
        c=classCreatorRest
        -> concatFive(e1={".new"},e2={$n.st},e3={$IDENTIFIER.text},e4={$t.st},e5={$c.st})
    ;

nonWildcardTypeArguments_wrapper
    :   n=nonWildcardTypeArguments
        -> copyStr(f={$n.st})
    |   -> eptStr()
    ;

classCreatorRest 
    :   a=arguments
        c=classBody_wrapper
        -> concatTwo(e1={$a.st}, e2={$c.st})
    ;

classBody_wrapper
    :   c=classBody
        -> copyStr(f={$c.st})
    |   -> eptStr()
    ;

nonWildcardTypeArguments 
    :   '<' t=typeList '>'
        -> concatThree(e1={"<"}, e2={$t.st}, e3={">"})
    ;

arguments 
    :   '(' e=expressionList_wrapper ')'
        -> concatThree(e1={"("}, e2={$e.st}, e3={")"})
    ;

expressionList_wrapper
    :   e=expressionList
        -> copyStr(f={$e.st})
    |   -> eptStr()
    ;

literal
    :   INTLITERAL
    |   LONGLITERAL
    |   FLOATLITERAL
    |   DOUBLELITERAL
    |   CHARLITERAL
    |   STRINGLITERAL
    |   TRUE
    |   FALSE
    |   NULL
    ;

oba_primary
    :   PERCENT o=oba_expression
        -> copyStr(f={$o.st})
    ;

oba_expression
    :   GET_TOKEN LPAREN s=signed_integer RPAREN
        -> getToken(index={$s.st})
    |   GET_PARSED_TEXT LPAREN RPAREN
        -> getParsedText(index={"0"})
    |   GET_PARSED_TEXT LPAREN s=signed_integer RPAREN
        -> getParsedText(index={$s.st})
    |   GET_PARSING_STACK_TRACE LPAREN RPAREN
        -> getParsingStackTrace()
    |   GET_BACKTRACK_LEVEL LPAREN RPAREN
        -> getBacktrackLevel()
    ;

signed_integer
    :   p=plusorminus_oneornull_wrapper INTLITERAL
        -> concatTwo(e1={$p.st},e2={$INTLITERAL.text})
    ;

plusorminus_oneornull_wrapper
    :   ('+'|'-')?
        -> copyStr(f={$text})
    ;


/**
 * These are headers help to make syntatiCFW predicates, not necessary but helps to make grammar faster.
 */
 
classHeader 
    :   modifiers 'class' IDENTIFIER
    ;

enumHeader 
    :   modifiers ('enum'|IDENTIFIER) IDENTIFIER
    ;

interfaceHeader 
    :   modifiers 'interface' IDENTIFIER
    ;

annotationHeader 
    :   modifiers '@' 'interface' IDENTIFIER
    ;

typeHeader 
    :   modifiers ('class'|'enum'|('@' ? 'interface')) IDENTIFIER
    ;

methodHeader 
    :   modifiers typeParameters? (type|'void')? IDENTIFIER '('
    ;

fieldHeader 
    :   modifiers type IDENTIFIER ('['']')* ('='|','|';')
    ;

localVariableHeader 
    :   variableModifiers type IDENTIFIER ('['']')* ('='|','|';')
    ;




/********************************************************************************************
                  Lexer section
*********************************************************************************************/

LONGLITERAL
    :   IntegerNumber LongSuffix
    ;

    
INTLITERAL
    :   IntegerNumber 
    ;
    
fragment
IntegerNumber
    :   '0' 
    |   '1'..'9' ('0'..'9')*    
    |   '0' ('0'..'7')+         
    |   HexPrefix HexDigit+        
    ;

fragment
HexPrefix
    :   '0x' | '0X'
    ;
        
fragment
HexDigit
    :   ('0'..'9'|'a'..'f'|'A'..'F')
    ;

fragment
LongSuffix
    :   'l' | 'L'
    ;


fragment
NonIntegerNumber
    :   ('0' .. '9')+ '.' ('0' .. '9')* Exponent?  
    |   '.' ( '0' .. '9' )+ Exponent?  
    |   ('0' .. '9')+ Exponent  
    |   ('0' .. '9')+ 
    |   
        HexPrefix (HexDigit )* 
        (    () 
        |    ('.' (HexDigit )* ) 
        ) 
        ( 'p' | 'P' ) 
        ( '+' | '-' )? 
        ( '0' .. '9' )+
        ;
        
fragment 
Exponent    
    :   ( 'e' | 'E' ) ( '+' | '-' )? ( '0' .. '9' )+ 
    ;
    
fragment 
FloatSuffix
    :   'f' | 'F' 
    ;     

fragment
DoubleSuffix
    :   'd' | 'D'
    ;
        
FLOATLITERAL
    :   NonIntegerNumber FloatSuffix
    ;
    
DOUBLELITERAL
    :   NonIntegerNumber DoubleSuffix?
    ;

CHARLITERAL
    :   '\'' 
        (   EscapeSequence 
        |   ~( '\'' | '\\' | '\r' | '\n' )
        ) 
        '\''
    ; 

STRINGLITERAL
    :   '"' 
        (   EscapeSequence
        |   ~( '\\' | '"' | '\r' | '\n' )        
        )* 
        '"' 
    ;

fragment
EscapeSequence 
    :   '\\' (
                 'b' 
             |   't' 
             |   'n' 
             |   'f' 
             |   'r' 
             |   '\"' 
             |   '\'' 
             |   '\\' 
             |       
                 ('0'..'3') ('0'..'7') ('0'..'7')
             |       
                 ('0'..'7') ('0'..'7') 
             |       
                 ('0'..'7')
             )          
;     

WS  
    :   (
             ' '
        |    '\r'
        |    '\t'
        |    '\u000C'
        |    '\n'
        ) 
            {
                skip();
            }          
    ;
    
COMMENT
         @init{
            boolean isJavaDoc = false;
        }
    :   '/*'
            {
                if((char)input.LA(1) == '*'){
                    isJavaDoc = true;
                }
            }
        (options {greedy=false;} : . )* 
        '*/'
            {
                if(isJavaDoc==true){
                    $channel=HIDDEN;
                }else{
                    skip();
                }
            }
    ;

LINE_COMMENT
    :   '//' ~('\n'|'\r')*  ('\r\n' | '\r' | '\n') 
            {
                skip();
            }
    |   '//' ~('\n'|'\r')*     // a line comment could appear at the end of the file without CR/LF
            {
                skip();
            }
    ;   
        
ABSTRACT
    :   'abstract'
    ;
    
ASSERT
    :   'assert'
    ;
    
BOOLEAN
    :   'boolean'
    ;
    
BREAK
    :   'break'
    ;
    
BYTE
    :   'byte'
    ;
    
CASE
    :   'case'
    ;
    
CATCH
    :   'catch'
    ;
    
CHAR
    :   'char'
    ;
    
CLASS
    :   'class'
    ;
    
CONST
    :   'const'
    ;

CONTINUE
    :   'continue'
    ;

DEFAULT
    :   'default'
    ;

DO
    :   'do'
    ;

DOUBLE
    :   'double'
    ;

ELSE
    :   'else'
    ;

ENUM
    :   'enum'
    ;             

EXTENDS
    :   'extends'
    ;

FINAL
    :   'final'
    ;

FINALLY
    :   'finally'
    ;

FLOAT
    :   'float'
    ;

FOR
    :   'for'
    ;

GOTO
    :   'goto'
    ;

IF
    :   'if'
    ;

IMPLEMENTS
    :   'implements'
    ;

IMPORT
    :   'import'
    ;

INSTANCEOF
    :   'instanceof'
    ;

INT
    :   'int'
    ;

INTERFACE
    :   'interface'
    ;

LONG
    :   'long'
    ;

NATIVE
    :   'native'
    ;

NEW
    :   'new'
    ;

PACKAGE
    :   'package'
    ;

PRIVATE
    :   'private'
    ;

PROTECTED
    :   'protected'
    ;

PUBLIC
    :   'public'
    ;

RETURN
    :   'return'
    ;

SHORT
    :   'short'
    ;

STATIC
    :   'static'
    ;

STRICTFP
    :   'strictfp'
    ;

SUPER
    :   'super'
    ;

SWITCH
    :   'switch'
    ;

SYNCHRONIZED
    :   'synchronized'
    ;

THIS
    :   'this'
    ;

THROW
    :   'throw'
    ;

THROWS
    :   'throws'
    ;

TRANSIENT
    :   'transient'
    ;

TRY
    :   'try'
    ;

VOID
    :   'void'
    ;

VOLATILE
    :   'volatile'
    ;

WHILE
    :   'while'
    ;

TRUE
    :   'true'
    ;

FALSE
    :   'false'
    ;

NULL
    :   'null'
    ;

LPAREN
    :   '('
    ;

RPAREN
    :   ')'
    ;

LBRACE
    :   '{'
    ;

RBRACE
    :   '}'
    ;

LBRACKET
    :   '['
    ;

RBRACKET
    :   ']'
    ;

SEMI
    :   ';'
    ;

COMMA
    :   ','
    ;

DOT
    :   '.'
    ;

ELLIPSIS
    :   '...'
    ;

EQ
    :   '='
    ;

TILDE
    :   '~'
    ;

QUES
    :   '?'
    ;

COLON
    :   ':'
    ;

EQEQ
    :   '=='
    ;

PLUSPLUS
    :   '++'
    ;

SUBSUB
    :   '--'
    ;

PLUS
    :   '+'
    ;

SUB
    :   '-'
    ;

STAR
    :   '*'
    ;

SLASH
    :   '/'
    ;

AMP
    :   '&'
    ;

BAR
    :   '|'
    ;

CARET
    :   '^'
    ;

PERCENT
    :   '%'
    ;

PLUSEQ
    :   '+='
    ; 
    
SUBEQ
    :   '-='
    ;

STAREQ
    :   '*='
    ;

SLASHEQ
    :   '/='
    ;

AMPEQ
    :   '&='
    ;

BAREQ
    :   '|='
    ;

CARETEQ
    :   '^='
    ;

PERCENTEQ
    :   '%='
    ;

BANGEQ
    :   '!='
    ;

GT
    :   '>'
    ;

LT
    :   '<'
    ;        
              
IDENTIFIER
    :   IdentifierStart IdentifierPart*
    ;

fragment
SurrogateIdentifer 
    :   ('\ud800'..'\udbff') ('\udc00'..'\udfff') 
    ;                 

fragment
IdentifierStart
    :   '\u0041'..'\u005a'
    |   '\u005f'
    |   '\u0061'..'\u007a'
    |   '\u00a2'..'\u00a5'
    |   '\u00aa'
    |   '\u00b5'
    |   '\u00ba'
    |   '\u00c0'..'\u00d6'
    |   '\u00d8'..'\u00f6'
    |   '\u00f8'..'\u0236'
    |   '\u0250'..'\u02c1'
    |   '\u02c6'..'\u02d1'
    |   '\u02e0'..'\u02e4'
    |   '\u02ee'
    |   '\u037a'
    |   '\u0386'
    |   '\u0388'..'\u038a'
    |   '\u038c'
    |   '\u038e'..'\u03a1'
    |   '\u03a3'..'\u03ce'
    |   '\u03d0'..'\u03f5'
    |   '\u03f7'..'\u03fb'
    |   '\u0400'..'\u0481'
    |   '\u048a'..'\u04ce'
    |   '\u04d0'..'\u04f5'
    |   '\u04f8'..'\u04f9'
    |   '\u0500'..'\u050f'
    |   '\u0531'..'\u0556'
    |   '\u0559'
    |   '\u0561'..'\u0587'
    |   '\u05d0'..'\u05ea'
    |   '\u05f0'..'\u05f2'
    |   '\u0621'..'\u063a'
    |   '\u0640'..'\u064a'
    |   '\u066e'..'\u066f'
    |   '\u0671'..'\u06d3'
    |   '\u06d5'
    |   '\u06e5'..'\u06e6'
    |   '\u06ee'..'\u06ef'
    |   '\u06fa'..'\u06fc'
    |   '\u06ff'
    |   '\u0710'
    |   '\u0712'..'\u072f'
    |   '\u074d'..'\u074f'
    |   '\u0780'..'\u07a5'
    |   '\u07b1'
    |   '\u0904'..'\u0939'
    |   '\u093d'
    |   '\u0950'
    |   '\u0958'..'\u0961'
    |   '\u0985'..'\u098c'
    |   '\u098f'..'\u0990'
    |   '\u0993'..'\u09a8'
    |   '\u09aa'..'\u09b0'
    |   '\u09b2'
    |   '\u09b6'..'\u09b9'
    |   '\u09bd'
    |   '\u09dc'..'\u09dd'
    |   '\u09df'..'\u09e1'
    |   '\u09f0'..'\u09f3'
    |   '\u0a05'..'\u0a0a'
    |   '\u0a0f'..'\u0a10'
    |   '\u0a13'..'\u0a28'
    |   '\u0a2a'..'\u0a30'
    |   '\u0a32'..'\u0a33'
    |   '\u0a35'..'\u0a36'
    |   '\u0a38'..'\u0a39'
    |   '\u0a59'..'\u0a5c'
    |   '\u0a5e'
    |   '\u0a72'..'\u0a74'
    |   '\u0a85'..'\u0a8d'
    |   '\u0a8f'..'\u0a91'
    |   '\u0a93'..'\u0aa8'
    |   '\u0aaa'..'\u0ab0'
    |   '\u0ab2'..'\u0ab3'
    |   '\u0ab5'..'\u0ab9'
    |   '\u0abd'
    |   '\u0ad0'
    |   '\u0ae0'..'\u0ae1'
    |   '\u0af1'
    |   '\u0b05'..'\u0b0c'
    |   '\u0b0f'..'\u0b10'
    |   '\u0b13'..'\u0b28'
    |   '\u0b2a'..'\u0b30'
    |   '\u0b32'..'\u0b33'
    |   '\u0b35'..'\u0b39'
    |   '\u0b3d'
    |   '\u0b5c'..'\u0b5d'
    |   '\u0b5f'..'\u0b61'
    |   '\u0b71'
    |   '\u0b83'
    |   '\u0b85'..'\u0b8a'
    |   '\u0b8e'..'\u0b90'
    |   '\u0b92'..'\u0b95'
    |   '\u0b99'..'\u0b9a'
    |   '\u0b9c'
    |   '\u0b9e'..'\u0b9f'
    |   '\u0ba3'..'\u0ba4'
    |   '\u0ba8'..'\u0baa'
    |   '\u0bae'..'\u0bb5'
    |   '\u0bb7'..'\u0bb9'
    |   '\u0bf9'
    |   '\u0c05'..'\u0c0c'
    |   '\u0c0e'..'\u0c10'
    |   '\u0c12'..'\u0c28'
    |   '\u0c2a'..'\u0c33'
    |   '\u0c35'..'\u0c39'
    |   '\u0c60'..'\u0c61'
    |   '\u0c85'..'\u0c8c'
    |   '\u0c8e'..'\u0c90'
    |   '\u0c92'..'\u0ca8'
    |   '\u0caa'..'\u0cb3'
    |   '\u0cb5'..'\u0cb9'
    |   '\u0cbd'
    |   '\u0cde'
    |   '\u0ce0'..'\u0ce1'
    |   '\u0d05'..'\u0d0c'
    |   '\u0d0e'..'\u0d10'
    |   '\u0d12'..'\u0d28'
    |   '\u0d2a'..'\u0d39'
    |   '\u0d60'..'\u0d61'
    |   '\u0d85'..'\u0d96'
    |   '\u0d9a'..'\u0db1'
    |   '\u0db3'..'\u0dbb'
    |   '\u0dbd'
    |   '\u0dc0'..'\u0dc6'
    |   '\u0e01'..'\u0e30'
    |   '\u0e32'..'\u0e33'
    |   '\u0e3f'..'\u0e46'
    |   '\u0e81'..'\u0e82'
    |   '\u0e84'
    |   '\u0e87'..'\u0e88'
    |   '\u0e8a'
    |   '\u0e8d'
    |   '\u0e94'..'\u0e97'
    |   '\u0e99'..'\u0e9f'
    |   '\u0ea1'..'\u0ea3'
    |   '\u0ea5'
    |   '\u0ea7'
    |   '\u0eaa'..'\u0eab'
    |   '\u0ead'..'\u0eb0'
    |   '\u0eb2'..'\u0eb3'
    |   '\u0ebd'
    |   '\u0ec0'..'\u0ec4'
    |   '\u0ec6'
    |   '\u0edc'..'\u0edd'
    |   '\u0f00'
    |   '\u0f40'..'\u0f47'
    |   '\u0f49'..'\u0f6a'
    |   '\u0f88'..'\u0f8b'
    |   '\u1000'..'\u1021'
    |   '\u1023'..'\u1027'
    |   '\u1029'..'\u102a'
    |   '\u1050'..'\u1055'
    |   '\u10a0'..'\u10c5'
    |   '\u10d0'..'\u10f8'
    |   '\u1100'..'\u1159'
    |   '\u115f'..'\u11a2'
    |   '\u11a8'..'\u11f9'
    |   '\u1200'..'\u1206'
    |   '\u1208'..'\u1246'
    |   '\u1248'
    |   '\u124a'..'\u124d'
    |   '\u1250'..'\u1256'
    |   '\u1258'
    |   '\u125a'..'\u125d'
    |   '\u1260'..'\u1286'
    |   '\u1288'
    |   '\u128a'..'\u128d'
    |   '\u1290'..'\u12ae'
    |   '\u12b0'
    |   '\u12b2'..'\u12b5'
    |   '\u12b8'..'\u12be'
    |   '\u12c0'
    |   '\u12c2'..'\u12c5'
    |   '\u12c8'..'\u12ce'
    |   '\u12d0'..'\u12d6'
    |   '\u12d8'..'\u12ee'
    |   '\u12f0'..'\u130e'
    |   '\u1310'
    |   '\u1312'..'\u1315'
    |   '\u1318'..'\u131e'
    |   '\u1320'..'\u1346'
    |   '\u1348'..'\u135a'
    |   '\u13a0'..'\u13f4'
    |   '\u1401'..'\u166c'
    |   '\u166f'..'\u1676'
    |   '\u1681'..'\u169a'
    |   '\u16a0'..'\u16ea'
    |   '\u16ee'..'\u16f0'
    |   '\u1700'..'\u170c'
    |   '\u170e'..'\u1711'
    |   '\u1720'..'\u1731'
    |   '\u1740'..'\u1751'
    |   '\u1760'..'\u176c'
    |   '\u176e'..'\u1770'
    |   '\u1780'..'\u17b3'
    |   '\u17d7' 
    |   '\u17db'..'\u17dc'
    |   '\u1820'..'\u1877'
    |   '\u1880'..'\u18a8'
    |   '\u1900'..'\u191c'
    |   '\u1950'..'\u196d'
    |   '\u1970'..'\u1974'
    |   '\u1d00'..'\u1d6b'
    |   '\u1e00'..'\u1e9b'
    |   '\u1ea0'..'\u1ef9'
    |   '\u1f00'..'\u1f15'
    |   '\u1f18'..'\u1f1d'
    |   '\u1f20'..'\u1f45'
    |   '\u1f48'..'\u1f4d'
    |   '\u1f50'..'\u1f57'
    |   '\u1f59'
    |   '\u1f5b'
    |   '\u1f5d'
    |   '\u1f5f'..'\u1f7d'
    |   '\u1f80'..'\u1fb4'
    |   '\u1fb6'..'\u1fbc'
    |   '\u1fbe'
    |   '\u1fc2'..'\u1fc4'
    |   '\u1fc6'..'\u1fcc'
    |   '\u1fd0'..'\u1fd3'
    |   '\u1fd6'..'\u1fdb'
    |   '\u1fe0'..'\u1fec'
    |   '\u1ff2'..'\u1ff4'
    |   '\u1ff6'..'\u1ffc'
    |   '\u203f'..'\u2040'
    |   '\u2054'
    |   '\u2071'
    |   '\u207f'
    |   '\u20a0'..'\u20b1'
    |   '\u2102'
    |   '\u2107'
    |   '\u210a'..'\u2113'
    |   '\u2115'
    |   '\u2119'..'\u211d'
    |   '\u2124'
    |   '\u2126'
    |   '\u2128'
    |   '\u212a'..'\u212d'
    |   '\u212f'..'\u2131'
    |   '\u2133'..'\u2139'
    |   '\u213d'..'\u213f'
    |   '\u2145'..'\u2149'
    |   '\u2160'..'\u2183'
    |   '\u3005'..'\u3007'
    |   '\u3021'..'\u3029'
    |   '\u3031'..'\u3035'
    |   '\u3038'..'\u303c'
    |   '\u3041'..'\u3096'
    |   '\u309d'..'\u309f'
    |   '\u30a1'..'\u30ff'
    |   '\u3105'..'\u312c'
    |   '\u3131'..'\u318e'
    |   '\u31a0'..'\u31b7'
    |   '\u31f0'..'\u31ff'
    |   '\u3400'..'\u4db5'
    |   '\u4e00'..'\u9fa5'
    |   '\ua000'..'\ua48c'
    |   '\uac00'..'\ud7a3'
    |   '\uf900'..'\ufa2d'
    |   '\ufa30'..'\ufa6a'
    |   '\ufb00'..'\ufb06'
    |   '\ufb13'..'\ufb17'
    |   '\ufb1d'
    |   '\ufb1f'..'\ufb28'
    |   '\ufb2a'..'\ufb36'
    |   '\ufb38'..'\ufb3c'
    |   '\ufb3e'
    |   '\ufb40'..'\ufb41'
    |   '\ufb43'..'\ufb44'
    |   '\ufb46'..'\ufbb1'
    |   '\ufbd3'..'\ufd3d'
    |   '\ufd50'..'\ufd8f'
    |   '\ufd92'..'\ufdc7'
    |   '\ufdf0'..'\ufdfc'
    |   '\ufe33'..'\ufe34'
    |   '\ufe4d'..'\ufe4f'
    |   '\ufe69'
    |   '\ufe70'..'\ufe74'
    |   '\ufe76'..'\ufefc'
    |   '\uff04'
    |   '\uff21'..'\uff3a'
    |   '\uff3f'
    |   '\uff41'..'\uff5a'
    |   '\uff65'..'\uffbe'
    |   '\uffc2'..'\uffc7'
    |   '\uffca'..'\uffcf'
    |   '\uffd2'..'\uffd7'
    |   '\uffda'..'\uffdc'
    |   '\uffe0'..'\uffe1'
    |   '\uffe5'..'\uffe6'
    |   ('\ud800'..'\udbff') ('\udc00'..'\udfff') 
    ;                
                       
fragment 
IdentifierPart
    :   '\u0000'..'\u0008'
    |   '\u000e'..'\u001b'
    |   '\u0024'
    |   '\u0030'..'\u0039'
    |   '\u0041'..'\u005a'
    |   '\u005f'
    |   '\u0061'..'\u007a'
    |   '\u007f'..'\u009f'
    |   '\u00a2'..'\u00a5'
    |   '\u00aa'
    |   '\u00ad'
    |   '\u00b5'
    |   '\u00ba'
    |   '\u00c0'..'\u00d6'
    |   '\u00d8'..'\u00f6'
    |   '\u00f8'..'\u0236'
    |   '\u0250'..'\u02c1'
    |   '\u02c6'..'\u02d1'
    |   '\u02e0'..'\u02e4'
    |   '\u02ee'
    |   '\u0300'..'\u0357'
    |   '\u035d'..'\u036f'
    |   '\u037a'
    |   '\u0386'
    |   '\u0388'..'\u038a'
    |   '\u038c'
    |   '\u038e'..'\u03a1'
    |   '\u03a3'..'\u03ce'
    |   '\u03d0'..'\u03f5'
    |   '\u03f7'..'\u03fb'
    |   '\u0400'..'\u0481'
    |   '\u0483'..'\u0486'
    |   '\u048a'..'\u04ce'
    |   '\u04d0'..'\u04f5'
    |   '\u04f8'..'\u04f9'
    |   '\u0500'..'\u050f'
    |   '\u0531'..'\u0556'
    |   '\u0559'
    |   '\u0561'..'\u0587'
    |   '\u0591'..'\u05a1'
    |   '\u05a3'..'\u05b9'
    |   '\u05bb'..'\u05bd'
    |   '\u05bf'
    |   '\u05c1'..'\u05c2'
    |   '\u05c4'
    |   '\u05d0'..'\u05ea'
    |   '\u05f0'..'\u05f2'
    |   '\u0600'..'\u0603'
    |   '\u0610'..'\u0615'
    |   '\u0621'..'\u063a'
    |   '\u0640'..'\u0658'
    |   '\u0660'..'\u0669'
    |   '\u066e'..'\u06d3'
    |   '\u06d5'..'\u06dd'
    |   '\u06df'..'\u06e8'
    |   '\u06ea'..'\u06fc'
    |   '\u06ff'
    |   '\u070f'..'\u074a'
    |   '\u074d'..'\u074f'
    |   '\u0780'..'\u07b1'
    |   '\u0901'..'\u0939'
    |   '\u093c'..'\u094d'
    |   '\u0950'..'\u0954'
    |   '\u0958'..'\u0963'
    |   '\u0966'..'\u096f'
    |   '\u0981'..'\u0983'
    |   '\u0985'..'\u098c'
    |   '\u098f'..'\u0990'
    |   '\u0993'..'\u09a8'
    |   '\u09aa'..'\u09b0'
    |   '\u09b2'
    |   '\u09b6'..'\u09b9'
    |   '\u09bc'..'\u09c4'
    |   '\u09c7'..'\u09c8'
    |   '\u09cb'..'\u09cd'
    |   '\u09d7'
    |   '\u09dc'..'\u09dd'
    |   '\u09df'..'\u09e3'
    |   '\u09e6'..'\u09f3'
    |   '\u0a01'..'\u0a03'
    |   '\u0a05'..'\u0a0a'
    |   '\u0a0f'..'\u0a10'
    |   '\u0a13'..'\u0a28'
    |   '\u0a2a'..'\u0a30'
    |   '\u0a32'..'\u0a33'
    |   '\u0a35'..'\u0a36'
    |   '\u0a38'..'\u0a39'
    |   '\u0a3c'
    |   '\u0a3e'..'\u0a42'
    |   '\u0a47'..'\u0a48'
    |   '\u0a4b'..'\u0a4d'
    |   '\u0a59'..'\u0a5c'
    |   '\u0a5e'
    |   '\u0a66'..'\u0a74'
    |   '\u0a81'..'\u0a83'
    |   '\u0a85'..'\u0a8d'
    |   '\u0a8f'..'\u0a91'
    |   '\u0a93'..'\u0aa8'
    |   '\u0aaa'..'\u0ab0'
    |   '\u0ab2'..'\u0ab3'
    |   '\u0ab5'..'\u0ab9'
    |   '\u0abc'..'\u0ac5'
    |   '\u0ac7'..'\u0ac9'
    |   '\u0acb'..'\u0acd'
    |   '\u0ad0'
    |   '\u0ae0'..'\u0ae3'
    |   '\u0ae6'..'\u0aef'
    |   '\u0af1'
    |   '\u0b01'..'\u0b03'
    |   '\u0b05'..'\u0b0c'        
    |   '\u0b0f'..'\u0b10'
    |   '\u0b13'..'\u0b28'
    |   '\u0b2a'..'\u0b30'
    |   '\u0b32'..'\u0b33'
    |   '\u0b35'..'\u0b39'
    |   '\u0b3c'..'\u0b43'
    |   '\u0b47'..'\u0b48'
    |   '\u0b4b'..'\u0b4d'
    |   '\u0b56'..'\u0b57'
    |   '\u0b5c'..'\u0b5d'
    |   '\u0b5f'..'\u0b61'
    |   '\u0b66'..'\u0b6f'
    |   '\u0b71'
    |   '\u0b82'..'\u0b83'
    |   '\u0b85'..'\u0b8a'
    |   '\u0b8e'..'\u0b90'
    |   '\u0b92'..'\u0b95'
    |   '\u0b99'..'\u0b9a'
    |   '\u0b9c'
    |   '\u0b9e'..'\u0b9f'
    |   '\u0ba3'..'\u0ba4'
    |   '\u0ba8'..'\u0baa'
    |   '\u0bae'..'\u0bb5'
    |   '\u0bb7'..'\u0bb9'
    |   '\u0bbe'..'\u0bc2'
    |   '\u0bc6'..'\u0bc8'
    |   '\u0bca'..'\u0bcd'
    |   '\u0bd7'
    |   '\u0be7'..'\u0bef'
    |   '\u0bf9'
    |   '\u0c01'..'\u0c03'
    |   '\u0c05'..'\u0c0c'
    |   '\u0c0e'..'\u0c10'
    |   '\u0c12'..'\u0c28'
    |   '\u0c2a'..'\u0c33'
    |   '\u0c35'..'\u0c39'
    |   '\u0c3e'..'\u0c44'
    |   '\u0c46'..'\u0c48'
    |   '\u0c4a'..'\u0c4d'
    |   '\u0c55'..'\u0c56'
    |   '\u0c60'..'\u0c61'
    |   '\u0c66'..'\u0c6f'        
    |   '\u0c82'..'\u0c83'
    |   '\u0c85'..'\u0c8c'
    |   '\u0c8e'..'\u0c90'
    |   '\u0c92'..'\u0ca8'
    |   '\u0caa'..'\u0cb3'
    |   '\u0cb5'..'\u0cb9'
    |   '\u0cbc'..'\u0cc4'
    |   '\u0cc6'..'\u0cc8'
    |   '\u0cca'..'\u0ccd'
    |   '\u0cd5'..'\u0cd6'
    |   '\u0cde'
    |   '\u0ce0'..'\u0ce1'
    |   '\u0ce6'..'\u0cef'
    |   '\u0d02'..'\u0d03'
    |   '\u0d05'..'\u0d0c'
    |   '\u0d0e'..'\u0d10'
    |   '\u0d12'..'\u0d28'
    |   '\u0d2a'..'\u0d39'
    |   '\u0d3e'..'\u0d43'
    |   '\u0d46'..'\u0d48'
    |   '\u0d4a'..'\u0d4d'
    |   '\u0d57'
    |   '\u0d60'..'\u0d61'
    |   '\u0d66'..'\u0d6f'
    |   '\u0d82'..'\u0d83'
    |   '\u0d85'..'\u0d96'
    |   '\u0d9a'..'\u0db1'
    |   '\u0db3'..'\u0dbb'
    |   '\u0dbd'
    |   '\u0dc0'..'\u0dc6'
    |   '\u0dca'
    |   '\u0dcf'..'\u0dd4'
    |   '\u0dd6'
    |   '\u0dd8'..'\u0ddf'
    |   '\u0df2'..'\u0df3'
    |   '\u0e01'..'\u0e3a'
    |   '\u0e3f'..'\u0e4e'
    |   '\u0e50'..'\u0e59'
    |   '\u0e81'..'\u0e82'
    |   '\u0e84'
    |   '\u0e87'..'\u0e88'        
    |   '\u0e8a'
    |   '\u0e8d'
    |   '\u0e94'..'\u0e97'
    |   '\u0e99'..'\u0e9f'
    |   '\u0ea1'..'\u0ea3'
    |   '\u0ea5'
    |   '\u0ea7'
    |   '\u0eaa'..'\u0eab'
    |   '\u0ead'..'\u0eb9'
    |   '\u0ebb'..'\u0ebd'
    |   '\u0ec0'..'\u0ec4'
    |   '\u0ec6'
    |   '\u0ec8'..'\u0ecd'
    |   '\u0ed0'..'\u0ed9'
    |   '\u0edc'..'\u0edd'
    |   '\u0f00'
    |   '\u0f18'..'\u0f19'
    |   '\u0f20'..'\u0f29'
    |   '\u0f35'
    |   '\u0f37'
    |   '\u0f39'
    |   '\u0f3e'..'\u0f47'
    |   '\u0f49'..'\u0f6a'
    |   '\u0f71'..'\u0f84'
    |   '\u0f86'..'\u0f8b'
    |   '\u0f90'..'\u0f97'
    |   '\u0f99'..'\u0fbc'
    |   '\u0fc6'
    |   '\u1000'..'\u1021'
    |   '\u1023'..'\u1027'
    |   '\u1029'..'\u102a'
    |   '\u102c'..'\u1032'
    |   '\u1036'..'\u1039'
    |   '\u1040'..'\u1049'
    |   '\u1050'..'\u1059'
    |   '\u10a0'..'\u10c5'
    |   '\u10d0'..'\u10f8'
    |   '\u1100'..'\u1159'
    |   '\u115f'..'\u11a2'
    |   '\u11a8'..'\u11f9'
    |   '\u1200'..'\u1206'        
    |   '\u1208'..'\u1246'
    |   '\u1248'
    |   '\u124a'..'\u124d'
    |   '\u1250'..'\u1256'
    |   '\u1258'
    |   '\u125a'..'\u125d'
    |   '\u1260'..'\u1286'
    |   '\u1288'        
    |   '\u128a'..'\u128d'
    |   '\u1290'..'\u12ae'
    |   '\u12b0'
    |   '\u12b2'..'\u12b5'
    |   '\u12b8'..'\u12be'
    |   '\u12c0'
    |   '\u12c2'..'\u12c5'
    |   '\u12c8'..'\u12ce'
    |   '\u12d0'..'\u12d6'
    |   '\u12d8'..'\u12ee'
    |   '\u12f0'..'\u130e'
    |   '\u1310'
    |   '\u1312'..'\u1315'
    |   '\u1318'..'\u131e'
    |   '\u1320'..'\u1346'
    |   '\u1348'..'\u135a'
    |   '\u1369'..'\u1371'
    |   '\u13a0'..'\u13f4'
    |   '\u1401'..'\u166c'
    |   '\u166f'..'\u1676'
    |   '\u1681'..'\u169a'
    |   '\u16a0'..'\u16ea'
    |   '\u16ee'..'\u16f0'
    |   '\u1700'..'\u170c'
    |   '\u170e'..'\u1714'
    |   '\u1720'..'\u1734'
    |   '\u1740'..'\u1753'
    |   '\u1760'..'\u176c'
    |   '\u176e'..'\u1770'
    |   '\u1772'..'\u1773'
    |   '\u1780'..'\u17d3'
    |   '\u17d7'
    |   '\u17db'..'\u17dd'
    |   '\u17e0'..'\u17e9'
    |   '\u180b'..'\u180d'
    |   '\u1810'..'\u1819'
    |   '\u1820'..'\u1877'
    |   '\u1880'..'\u18a9'
    |   '\u1900'..'\u191c'
    |   '\u1920'..'\u192b'
    |   '\u1930'..'\u193b'
    |   '\u1946'..'\u196d'
    |   '\u1970'..'\u1974'
    |   '\u1d00'..'\u1d6b'
    |   '\u1e00'..'\u1e9b'
    |   '\u1ea0'..'\u1ef9'
    |   '\u1f00'..'\u1f15'
    |   '\u1f18'..'\u1f1d'
    |   '\u1f20'..'\u1f45'
    |   '\u1f48'..'\u1f4d'
    |   '\u1f50'..'\u1f57'
    |   '\u1f59'
    |   '\u1f5b'
    |   '\u1f5d'
    |   '\u1f5f'..'\u1f7d'
    |   '\u1f80'..'\u1fb4'
    |   '\u1fb6'..'\u1fbc'        
    |   '\u1fbe'
    |   '\u1fc2'..'\u1fc4'
    |   '\u1fc6'..'\u1fcc'
    |   '\u1fd0'..'\u1fd3'
    |   '\u1fd6'..'\u1fdb'
    |   '\u1fe0'..'\u1fec'
    |   '\u1ff2'..'\u1ff4'
    |   '\u1ff6'..'\u1ffc'
    |   '\u200c'..'\u200f'
    |   '\u202a'..'\u202e'
    |   '\u203f'..'\u2040'
    |   '\u2054'
    |   '\u2060'..'\u2063'
    |   '\u206a'..'\u206f'
    |   '\u2071'
    |   '\u207f'
    |   '\u20a0'..'\u20b1'
    |   '\u20d0'..'\u20dc'
    |   '\u20e1'
    |   '\u20e5'..'\u20ea'
    |   '\u2102'
    |   '\u2107'
    |   '\u210a'..'\u2113'
    |   '\u2115'
    |   '\u2119'..'\u211d'
    |   '\u2124'
    |   '\u2126'
    |   '\u2128'
    |   '\u212a'..'\u212d'
    |   '\u212f'..'\u2131'
    |   '\u2133'..'\u2139'
    |   '\u213d'..'\u213f'
    |   '\u2145'..'\u2149'
    |   '\u2160'..'\u2183'
    |   '\u3005'..'\u3007'
    |   '\u3021'..'\u302f'        
    |   '\u3031'..'\u3035'
    |   '\u3038'..'\u303c'
    |   '\u3041'..'\u3096'
    |   '\u3099'..'\u309a'
    |   '\u309d'..'\u309f'
    |   '\u30a1'..'\u30ff'
    |   '\u3105'..'\u312c'
    |   '\u3131'..'\u318e'
    |   '\u31a0'..'\u31b7'
    |   '\u31f0'..'\u31ff'
    |   '\u3400'..'\u4db5'
    |   '\u4e00'..'\u9fa5'
    |   '\ua000'..'\ua48c'
    |   '\uac00'..'\ud7a3'
    |   '\uf900'..'\ufa2d'
    |   '\ufa30'..'\ufa6a'
    |   '\ufb00'..'\ufb06'
    |   '\ufb13'..'\ufb17'
    |   '\ufb1d'..'\ufb28'
    |   '\ufb2a'..'\ufb36'
    |   '\ufb38'..'\ufb3c'
    |   '\ufb3e'
    |   '\ufb40'..'\ufb41'
    |   '\ufb43'..'\ufb44'
    |   '\ufb46'..'\ufbb1'
    |   '\ufbd3'..'\ufd3d'
    |   '\ufd50'..'\ufd8f'
    |   '\ufd92'..'\ufdc7'
    |   '\ufdf0'..'\ufdfc'
    |   '\ufe00'..'\ufe0f'
    |   '\ufe20'..'\ufe23'
    |   '\ufe33'..'\ufe34'
    |   '\ufe4d'..'\ufe4f'
    |   '\ufe69'
    |   '\ufe70'..'\ufe74'
    |   '\ufe76'..'\ufefc'
    |   '\ufeff'
    |   '\uff04'
    |   '\uff10'..'\uff19'
    |   '\uff21'..'\uff3a'
    |   '\uff3f'
    |   '\uff41'..'\uff5a'
    |   '\uff65'..'\uffbe'
    |   '\uffc2'..'\uffc7'
    |   '\uffca'..'\uffcf'
    |   '\uffd2'..'\uffd7'
    |   '\uffda'..'\uffdc'
    |   '\uffe0'..'\uffe1'
    |   '\uffe5'..'\uffe6'
    |   '\ufff9'..'\ufffb' 
    |   ('\ud800'..'\udbff') ('\udc00'..'\udfff')
    ;

