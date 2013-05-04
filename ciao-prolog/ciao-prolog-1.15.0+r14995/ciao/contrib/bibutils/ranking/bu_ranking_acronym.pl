:- module(bu_ranking_acronym,[available_ranking_acronym/1, ranking_acronym/2]).

available_ranking_acronym(Es) :-
    setof(E,X^ranking_acronym(E,X),Es).

ranking_acronym(Acronym,String1) :-
    ranking_acronym_aux(Acronym,String2), 
    match(String1, String2),!.

%match(X, X).

match("Proc." || S1, S2):- match_(S1, S2).	
match("Proc.of" || S1, S2):- match_(S1, S2).	
match("Proc.ofthe" || S1, S2):- match_(S1, S2).	
match("Proceedingsof" || S1, S2):- match_(S1, S2).
match("Proceedingsofthe" || S1, S2):- match_(S1, S2).
match(S1, S2):- 
	match_(S1, S2).

match_("Proceedings", []).
match_("1st"||T1, "0th"||T2):- match_(T1, T2).
match_("2nd"||T1, "0th"||T2):- match_(T1, T2).
match_("3rd"||T1, "0th"||T2):- match_(T1, T2).
match_("Intl."||T1, "International"||T2):- match_(T1, T2).
match_("Conf."||T1, "Conference"||T2):- match_(T1, T2).
match_([H1|T1] , [H2|T2]):- 
	(
	    H1 = H2, !;

	    % not case sensitive 
	    ( 0'A =< H1, H1 =< 0'Z, H2 is H1 + ( 0'a- 0'A)), !; 
	    ( 0'a =< H1, H1 =< 0'z, H2 is H1 + ( 0'A- 0'a)), !;

	    % matchs any number to any number
	    ( 0'0 =< H1, H1 =< 0'9, 0'0 =< H2, H2 =< 0'9), !
	),
	match_(T1, T2).
match_([], []).




%%%%%
%%%%% conferences
%%%%%






ranking_acronym_aux(acm_com ,"CommunicationsoftheACM").
ranking_acronym_aux(sigplan_notices, "ACMSIGPLANNotices").
ranking_acronym_aux(amast, "AlgebraicMethodologyandSoftwareTechnology00thInternationalConferenceAMAST0000Proceedings").
ranking_acronym_aux(alp,"0000InternationalConferenceonAlgebraicandLogicProgramming").
ranking_acronym_aux(alp,"FourthInternationalConferenceonAlgebraicandLogicProgramming").

ranking_acronym_aux(aplas,"TheSeventhAsianSymposiumonProgrammingLanguagesandSystemsAPLAS09").

ranking_acronym_aux(asian,"NinthAsianComputingScienceConferenceASIAN00").
ranking_acronym_aux(atva ,"Proceedingsofthe4thInternationalSymposiumonAutomatedTechnologyforVerificationandAnalysisATVA06").
ranking_acronym_aux(cases,"CompilersArchitectureandSynthesisforEmbeddedSystems").
ranking_acronym_aux(cc,"InternationalConferenceonCompilerConstructionCC0000").
ranking_acronym_aux(ciclops,"CICLOPS0000").
ranking_acronym_aux(cl,"InternationalConferenceonComputationalLogicCL0000").
ranking_acronym_aux(cocv,"CompilerOptimizationmeetsCompilerVerificationCOCV00").
ranking_acronym_aux(cp,"PrinciplesandPracticeofConstraintProgramming").
ranking_acronym_aux(cp,"InternationalConferenceonPrinciplesandPracticeofConstraintProgrammingCP00").
ranking_acronym_aux(csc,"0000ACMComputerScienceConference").
ranking_acronym_aux(damp,"DAMP00ACMSIGPLANWorkshoponDeclarativeAspectsofMulticoreProgramming").
ranking_acronym_aux(dpmc,"DPMCIntelWorkshoponDeclarativeProgrammingLanguagesforMulticoreProgramming").
ranking_acronym_aux(dri,"DiagrammaticRepresentationandInference").

ranking_acronym_aux(esop,"00thEuropeanSymposiumonProgrammingESOP00").
ranking_acronym_aux(esop,"ESOP").
ranking_acronym_aux(esop,"EuropeanSymposiumonProgramming").
ranking_acronym_aux(esop,"ESOP00").
ranking_acronym_aux(esop,"ESOP0000").
ranking_acronym_aux(europar,"EUROPAR00").
ranking_acronym_aux(europar,"EUROPAR0000").
ranking_acronym_aux(europar,"EUROPAR00").
ranking_acronym_aux(europar,"EUROPAR00").
ranking_acronym_aux(europar,"EuroPar00").
%ranking_acronym_aux(evolang,"EvolutionofLanguageFifthInternationalConference").
ranking_acronym_aux(fgcs,"0000InternationalConferenceonFifthGenerationComputerSystems").
%ranking_acronym_aux(figc,"IGC0000SecondInternationalWorkshoponInteractiveGraphicalCommunication").
ranking_acronym_aux(flops,"FunctionalandLogicProgrammingFLOPS08").
ranking_acronym_aux(flops,"0thInternationalSymposiumonFunctionalandLogicProgrammingFLOPS0000").
ranking_acronym_aux(flops,"FLOPS00").
ranking_acronym_aux(flops,"The0thInternationalSymposiumonFunctionalandLogicProgrammingFLOPS00").
ranking_acronym_aux(flops ,"Proc.9thInternationalSymposiumonFunctionalandLogicProgrammingFLOPS08").
ranking_acronym_aux(fmco,"PostproceedingsofFormalMethodsforComponentsandObjectsFMCO07").
ranking_acronym_aux(fmoods,"IFIPInternationalConferenceonFormalMethodsforOpenObjectbasedDistributedSystemsFMOODS").
ranking_acronym_aux(fmoods,"16thInternationalSymposiumonFormalMethodsFM09").
ranking_acronym_aux(fmoods,"FMOODS").
ranking_acronym_aux(forte, "FormalTechniquesforNetworkedandDistributedSystemsFORTE0000").
ranking_acronym_aux(hosc ,"HigherOrderandSymbolicComputation").
ranking_acronym_aux(ic,"InformationandComputation").
ranking_acronym_aux(icalp,"00th.InternationalColloqiumonAutomataLanguagesandProgrammingICALP").
ranking_acronym_aux(icatpn, "00thInternationalConferenceonApplicationsandTheoryofPetriNetsandOtherModelsofConcurrencyICATPN0000").
ranking_acronym_aux(icfp ,"Proceedingsofthe12thACMSIGPLANInternationalconferenceonfunctionalprogrammingICFP2007").
ranking_acronym_aux(iclp,"0000InternationalConferenceonLogicProgramming").
ranking_acronym_aux(iclp,"0000ICLP").
ranking_acronym_aux(iclp,"ICLP0000").
ranking_acronym_aux(iclp,"0000InternationalConferenceonLogicProgramming").
ranking_acronym_aux(iclp,"00thInternationalConferenceonLogicProgrammingICLP0000").
ranking_acronym_aux(iclp,"00thInternationalConferenceonLogicProgrammingICLP0000").
ranking_acronym_aux(iclp,"0th.ICLP").
ranking_acronym_aux(iclp,"FifthInternationalConferenceonLogicProgrammingandAutomatedReasoning").
ranking_acronym_aux(iclp,"FourthInternationalConferenceonLogicProgramming").
ranking_acronym_aux(iclp,"ICLP").
ranking_acronym_aux(iclp,"ICLP00").
ranking_acronym_aux(iclp,"InternationalConferenceonLogicProgramming").
ranking_acronym_aux(iclp,"InternationalConferenceonLogicProgrammingICLP00").
ranking_acronym_aux(iclp,"InternationalConferenceonLogicProgrammingICLP0000").
ranking_acronym_aux(iclp,"InternationalLogicProgrammingSymposium").
ranking_acronym_aux(iclp,"InternationalConferenceonLogicProgrammingICLP").
ranking_acronym_aux(iclp,"LogicProgramming00thInternationalConferenceICLP0000SeattleWAUSAAugust00000000Proceedings").
ranking_acronym_aux(iclp,"ICLP00").
ranking_acronym_aux(iclp,"InternationalConferenceonLogicProgrammingICLP00").
ranking_acronym_aux(iclp,"EigthInternationalConferenceonLogicProgrammingandAutomatedReasoning").
ranking_acronym_aux(iclp,"InternationalConferenceonLogicProgrammingICLP").
ranking_acronym_aux(iclp,"TwelfthInternationalConferenceonLogicProgramming").
ranking_acronym_aux(iclp,"ThirdInternationalConferenceonLogicProgramming").
ranking_acronym_aux(iclp,"ThirdIntl.Conf.onLogicProgramming").
ranking_acronym_aux(iclp,"00thInternationalConferenceonLogicProgrammingICLP00").

ranking_acronym_aux(icpp,"0000IEEEInternationalConferenceonParallelProcessing").
ranking_acronym_aux(icpp,"00thInternationalConferenceonParallelProcessing").
ranking_acronym_aux(ictac, "ICTAC").
ranking_acronym_aux(ictcs,"ItalianConferenceonTheoreticalComputerScienceICTCS").
ranking_acronym_aux(icsoc,"ProceedingsoftheSixthInternationalConferenceonServiceOrientedComputing").
ranking_acronym_aux(ifl, "00thInternationalSymposiumontheImplementationofFunctionalLanguagesIFL0000").
ranking_acronym_aux(ijaamas ,"InternationalJournalofAutonomousAgentsandMultAgentSystemsSpecialIssueonNorms").
ranking_acronym_aux(ilps,"0000InternationalLogicProgrammingSymposium").
ranking_acronym_aux(ilps,"0000InternationalLogicProgrammingSymposium").
ranking_acronym_aux(ilps,"0000InternationalSymposiumonLogicProgramming").
ranking_acronym_aux(ilps,"0000InternationalLogicProgrammingSymposium").
ranking_acronym_aux(ilps,"0000InternationalLogicProgrammingSymposium").
ranking_acronym_aux(ilps,"FifthInternationalConferenceandSymposiumonLogicProgramming").
ranking_acronym_aux(ilps,"ILPS00").
ranking_acronym_aux(ilps,"ILPS00WSonToolsandEnvironmentsfor(C)LP").
ranking_acronym_aux(ilps,"InternationalLogicProgrammingSymposium").
ranking_acronym_aux(ilps,"InternationalSymposiumonLogicProgramming").
ranking_acronym_aux(ilps,"ILPS00").
ranking_acronym_aux(ilps,"ILPS00").
ranking_acronym_aux(ilps,"00thInternationalLogicProgrammingSymposium").
ranking_acronym_aux(ilps,"ILPS00WorkshoponParallelLogicProgrammingSystems").
ranking_acronym_aux(ilps,"ILPS00WorkshoponVisionsfortheFutureofLogicProgramming").
ranking_acronym_aux(ilps,"ILPS00WorkshoponToolsandEnvironmentsfor(Constraint)LogicProgramming").
ranking_acronym_aux(ipps,"InternationalParallelProcessingSymposium").
ranking_acronym_aux(ismm,"ISMM00").
ranking_acronym_aux(ismm,"ISMM00Proceedingsofthe0thinternationalsymposiumonMemorymanagement").
ranking_acronym_aux(ist,"InformationandSoftwareTechnologySpecialissueonModelBasedDevelopmentforSecureInformationSystems").
ranking_acronym_aux(jelia,"00thEuropeanConferenceonLogicsinArtificialIntelligenceJELIA00").
%ranking_acronym_aux(laci, "XXXIILatinAmericanConferenceonInformatics").
ranking_acronym_aux(lcpc,"LanguagesandCompilersforParallelComputingLCPC00").
ranking_acronym_aux(lopstr,"00thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"00thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"00thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"LOPSTR00").
ranking_acronym_aux(lopstr,"LogicBasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"LogicProgramSynthesisandTransformation").
ranking_acronym_aux(lopstr,"LogicProgramSynthesisandTransformation0000").
ranking_acronym_aux(lopstr,"LogicbasedProgramSynthesisandTransformation").
ranking_acronym_aux(lopstr,"LogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"Preproceedingsofthe00thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"Preproceedingsofthe00thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"LOPSTR00").
ranking_acronym_aux(lopstr,"00thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"TenthInternationalWorkshoponLogicbasedProgramSynthesisandTransformation").
ranking_acronym_aux(lopstr,"WorkshoponLogicProgramSynthesisandTransformation").
ranking_acronym_aux(lopstr,"00thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR00").
ranking_acronym_aux(lopstr,"8thInternationalWorkingConferenceonSourceCodeAnalysisandManipulationSCAM08").
ranking_acronym_aux(lopstr,"17thInternationalSymposiumonLogicbasedProgramSynthesisandTransformationLOPSTR2007").
ranking_acronym_aux(lpar,"00thInternationalConferenceonLogicforProgrammingArtificialIntelligenceandReasoningLPAR00").
ranking_acronym_aux(lpar,"00thInternationalConferenceonLogicforProgrammingArtificialIntelligenceandReasoningLPAR00").
ranking_acronym_aux(lpar,"00thIntl.ConferenceonLogicforProgrammingArtificialIntelligenceandReasoningLPAR00").
ranking_acronym_aux(lpar,"0thInternationalConferenceonLogicforProgrammingArtificialIntelligenceandReasoning").
ranking_acronym_aux(lpar,"FifthInternationalConferenceonAutomatedReasoningLPAR00").
ranking_acronym_aux(lpar,"FifthInternationalConferenceonLogicProgrammingandAutomatedReasoning").
ranking_acronym_aux(lpar,"LPAR00").
ranking_acronym_aux(lpar,"EigthInternationalConferenceonLogicProgrammingandAutomatedReasoning").
ranking_acronym_aux(lpar,"11thInternationalConferenceonLogicforProgrammingArtificialIntelligenceandReasoningLPAR2004").

ranking_acronym_aux(movelog,"MobileCodeSafetyandProgramVerificationUsingComputationalLogicToolsMoveLog00").
ranking_acronym_aux(naclp,"0000NorthAmericanConferenceonLogicProgramming").
ranking_acronym_aux(naclp,"0000NorthAmericanConferenceonLogicProgramming").
%ranking_acronym_aux(paclp,"ThePracticalApplicationofConstraintTechnologiesandLogicprogramming").
ranking_acronym_aux(padl,"SixthInternationalSymposiumonPracticalAspectsofDeclarativeLanguages").
ranking_acronym_aux(padl,"00thInternationalSymposiumonPracticalAspectsofDeclarativeLanguagesPADL00").
ranking_acronym_aux(padl,"00thInternationalSymposiumonPracticalAspectsofDeclarativeLanguagesPADL00").
ranking_acronym_aux(padl,"EightInternationalSymposiumonPracticalAspectsofDeclarativeLanguages").
ranking_acronym_aux(padl,"FourthInternationalSymposiumonPracticalAspectsofDeclarativeLanguages").
ranking_acronym_aux(padl,"FourthInternationalSymposiumonPracticalBAspectsofDeclarativeLanguagesPADL00").
ranking_acronym_aux(padl,"Intnl.SymposiumonPracticalAspectsofDeclarativeLanguages").
ranking_acronym_aux(padl,"NinthInternationalSymposiumonPracticalAspectsofDeclarativeLanguages").
ranking_acronym_aux(padl,"PADL00").
ranking_acronym_aux(padl,"PADL").
ranking_acronym_aux(padl,"PADL00").
ranking_acronym_aux(padl,"ProoceedingsoftheSixthInternationalSymposiumonPracticalAspectsofDeclarativeLanguages").
ranking_acronym_aux(padl,"NinthInternationalSymposiumonPracticalAspectsofDeclarativeLanguagesPADL2007").

ranking_acronym_aux(parle,"ParallelArchitecturesandLanguagesEuropePARLE00").
ranking_acronym_aux(pasco,"FirstInternationalSymposiumonParallelSymbolicComputation").
ranking_acronym_aux(pasco,"FirstInternationalSymposiumonParallelSymbolicComputation,PASCO94").
ranking_acronym_aux(pasco,"PASCO00").
ranking_acronym_aux(pasco,"ParallelSymbolicComputationPASCO00").
ranking_acronym_aux(pasco,"FirstInternationalSymposiumonParallelSymbolicComputation,PASCO00").
ranking_acronym_aux(pasco,"Proc.ofFirstInternationalSymposiumonParallelSymbolicComputationPASCO94").
ranking_acronym_aux(pasco,"FirstInternationalSymposiumonParallelSymbolicComputationPASCO94").
ranking_acronym_aux(paste,"ACMWorkshoponProgramAnalysisforSoftwareToolsandEngineeringPASTE00").
ranking_acronym_aux(paste,"ACMWSonProgramAnalysisforSoftwareToolsandEngineeringPASTE08").
ranking_acronym_aux(paste,"ACMWSonProgramAnalysisforSoftwareToolsandEngineeringPASTE07").

ranking_acronym_aux(pepm,"ACMPartialEvaluationandProgramManipulationPEPM00").
ranking_acronym_aux(pepm,"ACMPartialEvaluationandSemanticsbasedProgramManipulationPEPM00").
ranking_acronym_aux(pepm,"ACMSIGPLAN0000WorkshoponPartialEvaluationandProgramManipulationPEPM00").
ranking_acronym_aux(pepm,"ACMSIGPLANWorkshoponPartialEvaluationandSemanticsBasedProgramManipulationPEPM00").
ranking_acronym_aux(pepm,"PEPM00").
ranking_acronym_aux(pepm,"ACMSIGPLANSymposiumonPartialEvaluationandSemanticsBasedProgramManipulation").
ranking_acronym_aux(pepm,"PEPM00").
ranking_acronym_aux(pepm,"PEPM00").
ranking_acronym_aux(pepm,"PEPM00").
ranking_acronym_aux(pepm,"ACMSIGPLANWorkshoponPartialEvaluationandSemanticsbasedProgramManipulationPEPM").
ranking_acronym_aux(pepm,"ACMSIGPLANWorkshoponPartialEvaluationandSemanticsbasedProgramManipulationPEPM").

ranking_acronym_aux(pldi,"ACMSigplan00ConferenceonProgrammingLanguageDesignandImplementation").
ranking_acronym_aux(pldi,"PLDI00").
ranking_acronym_aux(pldi,"0000ACMConf.onProgrammingLanguageDesignandImplementation").
ranking_acronym_aux(pldi,"ACMSigplan00ConferenceonProgrammingLanguageDesignandImplementation").
ranking_acronym_aux(plilp,"ParallelismandImplementationofLogicandConstraintLogicProgramming").
ranking_acronym_aux(plilp,"0th.IntlSymposiumonProgrammingLanguageImplementationandLogicProgramming").
ranking_acronym_aux(plilp,"ProgrammingLanguagesImplementationLogicsandPrograms").
ranking_acronym_aux(popl,"00th.AnnualACMConf.onPrinciplesofProgrammingLanguages").
ranking_acronym_aux(ppdp,"Intnl.SymposiumonPracticalAspectsofDeclarativeLanguages").
ranking_acronym_aux(ppdp,"0thACMSIGPLANInternationalSymposiumonPrinciplesandPracticeofDeclarativeProgrammingPPDP00").
ranking_acronym_aux(ppdp,"0thACMSIGPLANInternationalSymposiumonPrinciplesandPracticeofDeclarativeProgrammingPPDP00").
ranking_acronym_aux(ppdp,"00thIntl.ACMSIGPLANSymposiumonPrinciplesandPracticeofDeclarativeProgrammingPPDP00").
ranking_acronym_aux(ppdp,"PPDP00").
ranking_acronym_aux(ppdp,"PPDP00").
ranking_acronym_aux(ppdp ,"PPDP00Proceedingsofthe12thACMSIGPLANinternationalconferenceonPrinciplesandpracticeofdeclaritiveprogramming").
ranking_acronym_aux(ppdp ,"00thInternationalACMSIGPLANSymposiumonPrinciplesandPracticeofDeclarativeProgrammingPPDP00").
ranking_acronym_aux(ppdp ,"PrinciplesandPracticeofDeclarativeProgrammingPPDP0000").

% ranking_acronym_aux(prole,"SpanishConferenceonProgrammingandComputerLanguagesPROLE08").
% ranking_acronym_aux(prole,"SpanishConferenceonProgrammingandComputerLanguagesPROLE09").

ranking_acronym_aux(sac,"ACMSACSoftwareVerificationTrackSV00").
ranking_acronym_aux(sac,"ACMSymposiumonAppliedComputingSAC").

ranking_acronym_aux(sara,"SymposiumonAbstractionReformulationandApproximationSARA0000").
ranking_acronym_aux(sas,"00thInternationalStaticAnalysisSymposiumSAS00").
ranking_acronym_aux(sas,"0000InternationalStaticAnalysisSymposium").
ranking_acronym_aux(sas,"InternationalStaticAnalysisSymposium").
ranking_acronym_aux(sas,"SAS00").
ranking_acronym_aux(sas,"SAS00").
ranking_acronym_aux(sas,"SAS00").
ranking_acronym_aux(sas,"FourthInternationalStaticAnalysisSymposium").
ranking_acronym_aux(sas,"SAS00").
ranking_acronym_aux(sas,"StaticAnalysisSymposiumSAS00").
ranking_acronym_aux(sas,"The00thInternationalStaticAnalysisSymposiumSAS00").
ranking_acronym_aux(sas,"StaticAnalysisSymposiumSAS").
ranking_acronym_aux(sas,"StaticAnalysis00thInternationalSymposiumSAS0000ValenciaSpainJuly00000000Proceedings").
%ranking_acronym_aux(ske,"0th.InternationalSymposiumonKnowledgeEngineering").
ranking_acronym_aux(smc,"0000IEEEConferenceonSystemsMan&CyberneticsSpecialSessiononCorrectnessandReliabilityforPervasive/UbiquitousComputing").
ranking_acronym_aux(sac,"ACMSymposiumonAppliedComputingSACSoftwareVerificationTrackSV00").
ranking_acronym_aux(scam,"SourceCodeAnalysisandManipulation").
ranking_acronym_aux(scam,"SCAM00").
ranking_acronym_aux(scam,"8thIEEEInternationalWorkingConferenceonSourceCodeAnalysisandManipulationSCAM08").
ranking_acronym_aux(sofsem, "00thInternationalConferenceonCurrentTrendsinTheoryandPracticeofComputerScienceSOFSEM00").
ranking_acronym_aux(iccs, "ICCS00Proceedingsofthe8thInternationalConferenceonComputationalScience").
ranking_acronym_aux(tgc,"TGC00").
ranking_acronym_aux(tgc,"TrustworthyGlobalComputing00").
ranking_acronym_aux(tlca,"TypedLambdaCalculusandApplicationsTLCA00").
ranking_acronym_aux(tlca,"Int.Conf.onTypedLambdaCalculiandApplicationsTLCA").

ranking_acronym_aux(vmcai,"0thInternationalConferenceonVerificationModelCheckingandAbstractInterpretationVMCAI00").

ranking_acronym_aux(ceur,"CEURWorkshopProceedings").
ranking_acronym_aux(entcs,"ElectronicNotesinTheoreticalComputerScience").
ranking_acronym_aux(entcs,"Electr.NotesTheor.Comput.Sci.").
ranking_acronym_aux(pc, "ParallelComputingCurrent&FutureIssuesofHighEndComputingProc.oftheInt.Conf.ParCo2005").
ranking_acronym_aux(plt,"FirstInternationalWorkshoponPenBasedLearningTechnologiesPLT0000").
ranking_acronym_aux(omt,"OTMConferences0").
ranking_acronym_aux(wits,"SixthWorkshoponIssuesintheTheoryofSecurity").
%ranking_acronym_aux(stfm,"SymposiumonTeachingFormalMethods0000").




%%%%%%
%%%%%% Journals
%%%%%%
ranking_acronym_aux(ipl,"InformationProcessingLetters").
ranking_acronym_aux(jist,"JournalofInformationandSoftwareTechnology").
ranking_acronym_aux(compsurv,"ACMComputingSurveys").
ranking_acronym_aux(toplas,"ACMTOPLAS").
ranking_acronym_aux(toplas,"ACMTrans.onProgrammingLanguagesandSystems").
ranking_acronym_aux(toplas,"ACMTransactionsonProgrammingLanguagesandSystems").
ranking_acronym_aux(tocl,"ACMTrans.Comput.Log.").
ranking_acronym_aux(cogsci,"CognitiveScience").
%ranking_acronym_aux(comparchnews, "ComputerArchitectureNewsSpecialIssueonParallelSymbolicProgramming").
ranking_acronym_aux(fac ,"FormalAspectsofComputing").
ranking_acronym_aux(swissinf,"Informatique/InformatikJournaloftheSwissInformaticsSocieties").
ranking_acronym_aux(ja ,"JournalofAlgorithms").
ranking_acronym_aux(jbcs ,"JournalofBrazilianComputerSocietySpecialIssueonSoftwareEngineeringforMultiAgentSystems").
ranking_acronym_aux(jlap,"JournalofLogicandAlgebraicProgramming").
ranking_acronym_aux(jlc,"J.ofLogicandComputation").
ranking_acronym_aux(jlc ,"JournalofLogicandComputation").
ranking_acronym_aux(jlp,"JLP").
ranking_acronym_aux(jlp,"J.ofLogicProgramming.SpecialIssueonSynthesisTransformationandAnalysisofLogicPrograms").
ranking_acronym_aux(jlp,"J.ofLogicProgramming").
ranking_acronym_aux(jlp,"JournalofLogicProgramming").
ranking_acronym_aux(jot ,"JOURNALofObjectTechnologyJOT").
ranking_acronym_aux(jpp,"Intl.JournalofParallelProgramming").
ranking_acronym_aux(jsc,"J.ofSymbolicComputationSpecialIssueonParallelSymbolicComputation").
ranking_acronym_aux(jsc,"JournalofSymbolicComputationSpecialIssueonParallelSymbolicComputation").
ranking_acronym_aux(jsc ,"JournalofSymbolicComputation").
ranking_acronym_aux(jsc,"J.ofSymbolicComputationSpecialIssueonParallelSymbolicComputation").
ranking_acronym_aux(jucs ,"JournalofUniversalComputerScience").
ranking_acronym_aux(newgc,"NewGenerationComputing").
ranking_acronym_aux(pc,"ParallelComputing").
ranking_acronym_aux(parco,"ParallelComputingConference").
ranking_acronym_aux(parco,"ConferenceOnParallelComputing").
ranking_acronym_aux(parco,"ConferencennParallelComputing").
ranking_acronym_aux(scp,"ScienceofComputerProgramming").
ranking_acronym_aux(clj,"TheComputerLanguagesJournal").
ranking_acronym_aux(constj,"TheConstraintsJournal").
ranking_acronym_aux(tlca ,"0thInternationalConferenceTypedLambdaCalculiandApplicationsTLCA0000").
ranking_acronym_aux(tlca ,"TypedLambdaCalculiandApplications").
ranking_acronym_aux(tcs,"TheoreticalComputerScience").
ranking_acronym_aux(tplp,"TheoryandPracticeofLogicProgramming").
ranking_acronym_aux(jssm,"J.onSoftware&SystemModeling").



