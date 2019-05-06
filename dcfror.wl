(* ::Package:: *)

(* ::Chapter:: *)
(*Discounted Cash Flow Code*)


NPV::usage="NPV[c,i] returns the net present value of cash flows c with discount rate i";
NPV[c_, i_]:=Module[{}, TimeValue[c,i, 0]]

AnnualValue::usage="AnnualValue[f, c, w, p] returns the annual values of cash flows f, capital and interest c, working capital w, equity e, and land l over project period p";
AnnualValue[f_, c_, w_, p_]:= Module[{investments, cashFlows, workingCapital}, 
	investments = c;
	cashFlows = Transpose[{ Range[Length@f], f}];
	workingCapital = {{p,w}};
	Cashflow[Join[
		investments,
		cashFlows,
		workingCapital]]]
		
Payment[c_,r_,t_]:=c*r*(1+r)^t/((1+r)^t-1)

LoanPaymentInterestPrincipal::usage="LoanPaymentInterestPrincipal[c, r, t, e] returns the loan payments for capital c at interest rate r over period t with equity e";
LoanPaymentInterestPrincipal[c_,r_,t_,e_]:=Transpose@
	NestList[
		Apply[Function[{p,i,pr},{p,pr*r,pr-p+pr*r}]],
		{Payment[c*(1-e),r,t], c*(1-e)*r, c*(1-e)+c*(1-e)*r-Payment[c*(1-e),r,t]},t-1]
	
CapitalandInterest::usage="CapitalandInterest[c, e, l, lr] returns the capital and interest paid for capital c, with equity e, land cost l, loan rate lr, and working capital w";
CapitalandInterest[c_,e_,l_,lr_,w_]:=Transpose[{{-2,-1,0},-c*e*{0.08, 0.6, 0.32} - {l,0,0} - Accumulate[c*(1-e)*lr*{0.08, 0.6, 0.32}]-{0,0,w}}] 
		
IncomeTax::usage="IncomeTax[i,r] returns the tax on income i at tax rate r";
IncomeTax[i_,r_:0.35]:=Map[If[#*r>0,#*r, 0]&,i]

TaxableIncome::usage="TaxableIncome[n] returns the taxable income given the net revenue n and losses l";
TaxableIncome[n_]:=FoldList[If[#1<0,#1+#2,#2]&,n]

NetRevenue::usage="NetRevenue[s,c,d] returns the net revenue from sales s, costs c, depreciation charges d, and loan interest payments l";
NetRevenue[s_,c_,d_, l_]:=s+c+d+l

AnnualSales::usage="AnnualSales[s, p, t, f] returns the annual sales s for periods p with startup time t and startup factor f";
AnnualSales[s_,p_,t_:0.25,f_:0.5]:=ReplacePart[ConstantArray[s,p],1->((1-t)*s + t*s*f)]

AnnualFixedCosts::usage="AnnualFixedCosts[s, p, t, f] returns the annual costs c for periods p with startup time t and startup factor f";
AnnualFixedCosts[c_,p_,t_:0.25,f_:1.0]:=ReplacePart[ConstantArray[c,p],1->((1-t)*c + t*c*f)]

AnnualVariableCosts::usage="AnnualVariableCosts[s, p, t, f] returns the annual costs s for periods p with startup time t and startup factor f";
AnnualVariableCosts[s_,p_,t_:0.25,f_:0.75]:=ReplacePart[ConstantArray[s,p],1->((1-t)*s + t*s*f)]

Sales::usage="Sales[o, p] returns the income from selling output o at price p";
Sales[o_,p_]:=o*p

VariableCosts::usage="Costs[m, p] returns the costs of using material m at price p";
VariableCosts[m_,p_]:=m*p

LaborCosts::usage="LaborCosts[n, s] returns the cost of employee n with salary s";
LaborCosts[n_, s_]:=n*s

OandMCosts::usage="OandMCosts[c, m, i] returns the overhead, maintenance, and insurance costs for capital c and labor l";
OandMCosts[l_,isbl_,c_ o_:0.9,m_:0.03,i_:0.007]:={l*o, isbl*m, c*i}

Depreciation::usage="Depreciation[c, p] returns the depreciation charges for capital c over periods p";
Depreciation[c_, 7]:=c*{0.1429`,0.2449`,0.1749`,0.1249`,0.0893`,0.0892`,0.0893`,0.0446`};

Depreciation[c_, 20]:=c*{0.0375`,0.0722`,0.0668`,0.0618`,0.0571`,0.0529`,0.0489`,0.0452`,0.0446`,0.0446`,0.0446`,0.0446`,0.0446`,0.0446`,0.0446`,0.0446`,0.0446`,0.0446`,0.0446`,0.0224`};


GetSales::usage="GetSales[p, pr, t] returns the total annual sales of products p at prices pr over time t";
GetSales[p_,pr_, t_]:= AnnualSales[Total@Sales[p,pr], t];

GetVariableCosts::usage="GetCosts[mat, pr, e, sal, t] returns the total annual costs of materials mat, prices pr, employs e with salaries sal over time t";
GetVariableCosts[mat_,pr_, t_]:= (
	AnnualSales[mat[[1]]*pr[[1]], t] +
	AnnualVariableCosts[Total@VariableCosts[Rest@mat, Rest@pr], t] +
	(If[#==1||Mod[#-1,5]==0,466183,0]&/@Range[t]))
	
GetFixedCosts::usage="GetFixedCosts[cap, isbl, e, s, t] returns the annual costs from capital cap, isbl, employees e, salaries s over time t";
GetFixedCosts[cap_,isbl_,e_, s_, t_]:=AnnualFixedCosts[Total@LaborCosts[e,s] +Total@OandMCosts[Total@LaborCosts[e,s], isbl, cap], t]

GetDepreciation::usage="GetDepreciation[cap, t] returns the depreciation cost of capital cap over period t for lifetime l";
GetDepreciation[cap_, t_, l_]:=PadRight[Depreciation[cap, t],l]

GetLoanPayments::usage="GetLoanPayments[c, i, t, e] returns the loan payment, interest, and principal for capital c at interest i over period t with equity e and lifetime l";
GetLoanPayments[c_,i_,t_,e_, l_]:=PadRight[#,l]&/@LoanPaymentInterestPrincipal[c, i, t, e]
	


GetNPV[data_]:=Module[{},
	irr=0.1;
	{pro, pri} = data["Products"][[{2,3}]];
	{mpl,mat,mpr}=data["Material Costs"][[{1,2,3}]];
	{emp, sal} = data["Salary Costs"][[{2,3}]];
	life=data["Project Lifetime"];
	hr = data["Operating Hours"];
	land = data["Land Cost"];
	loanI = data["Loan Interest"];
	loanP = data["Loan Term"];
	loanE = data["Equity"];
	wc = data["Working Capital"];
	cap = data["Fixed Capital"];
	isbl = data["ISBL"];
	gpc = data["General Plant Capital"];
	gpp = data["General Plant Period"];
	spc = data["Steam Plant Capital"];
	spp = data["Steam Plant Period"];
	
	If[
		Length@Select[Flatten@{irr, pro, pri, mat, mpr, emp, sal, life, hr, land, loanI, loanP, loanE, wc, cap, isbl, gpc, gpp, spc, spp}, 
			!NumericQ@#&]>0,
		Abort[]];

	sales = GetSales[pro*hr, pri,life];

	variable = GetVariableCosts[mat*hr, mpr, life];
	fixed = GetFixedCosts[cap, isbl, emp, sal,life];
	costs = variable + fixed;

	depreciation = GetDepreciation[gpc, gpp, life] + GetDepreciation[spc, spp,life];
	loans = GetLoanPayments[cap, loanI, loanP, loanE, life];

	capAndI = CapitalandInterest[cap, loanE, land, loanI, wc*cap];
	NR = NetRevenue[sales, -costs, -depreciation, -loans[[2]]];
	taxes = IncomeTax@TaxableIncome@NR;
	AV = AnnualValue[sales - costs - taxes -loans[[1]],capAndI, cap*wc+land, life];
	
	opexTable = Append[#,{"ROI", pro[[1]]*pri[[1]]*hr - Total[#[[All,2]]] }]&@
		Join[
			{mpl[[1;;]], mat[[1;;]]*mpr[[1;;]]*hr}\[Transpose],
			{{"Fixed", Mean@fixed}},
			{{"Depreciation", cap/life}},
			{{"Income Tax", Mean@taxes}}
		];
	<|"OPEX"->opexTable,"NPV"->NPV[AV,irr]|>
]

MFSP[mfsp0_?NumericQ,data_]:=Block[{mfsp = mfsp0}, GetNPV[data]["NPV"]];

GetMFSP[data_]:=First[FindRoot[MFSP[x, data],{x,1,0,10}],{0,1*^9}][[2]];


PositionsOfTopCosts::usage="Returns the positions of the top t values v";
PositionsOfTopCosts[v_,t_:6]:= Flatten[Position[v,#]&/@TakeLargestBy[v,Abs@# &,t],1]

SummarizeCosts::usage="Summarize a list of values v and labels l by taking the largest of up to the max";
SummarizeCosts[l_,v_,max_:6]:= Module[{p},
	p = PositionsOfTopCosts[v,max];
	Append[
		Extract[#,p],
		{"Other",Total[Delete[#,p][[All,2]]]}
		]&@Transpose[{l,v}]
	]

ChartCosts[l_,v_,max_:6,opt:OptionsPattern[]]:=Module[{lbls,vals},
	{lbls,vals}=SummarizeCosts[l,v,max]\[Transpose];
	BarChart[
		vals,
		ChartLegends->lbls,
		opt]
	]

SummarizeMultipleCosts[l_,vs_,max_:6]:=Module[{p, l2, vs2},
	p = PositionsOfTopCosts[vs[[2]],max];
	l2 = Append[Extract[l,p],"Other"];
	vs2 = Append[Extract[#,p],Total[Delete[#,p]]]&/@vs;
	Prepend[vs2,l2]
]

SummarizeMultipleCostsAssociation[l_,vs_,max_:6]:=Module[{ls, vs2},
	{ls,vs2} = {First@#,Rest@#}&@SummarizeMultipleCosts[l,vs,max];
	Association@MapThread[Rule,{ls,#}]&/@vs2
	
]

ChartMultipleCosts[l_,vs_,max_:6,opt:OptionsPattern[]]:=Module[{lbls,vals},
	{lbls,vals}=SummarizeMultipleCosts[l,vs[[2]],max]\[Transpose];
	BarChart[
		vs,
		ChartLegends->lbls,
		opt]
]

ChartMultipleCostsAssociation[l_,vs_,max_:6,opt:OptionsPattern[]]:=Module[{d},
	d=SummarizeMultipleCostsAssociation[l,vs,max];
	BarChart[d,opt]
]
