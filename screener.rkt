#lang racket


(require racket/system)

;return bash result as string
(define (read-command cmd)

  (let ( (response (with-output-to-string (lambda () (system cmd)))))

    (substring response 0 (- (string-length response) 1))
    )
  )

;+-0.01 precision - quote may be delayed
(define (stock-price symbol)

  (string->number (read-command (~a "curl -s 'http://download.finance.yahoo.com/d/quotes.csv?s=" symbol "&f=l1'")))
  
  )
;% change since market open
(define (stock-price-change symbol)
(let (( change
(string->number (read-command (~a "curl -s 'http://download.finance.yahoo.com/d/quotes.csv?s=" symbol "&f=c1'")))))
  ;if stock doesn't exist returns #f converted to 0% change
  (if change change 0))
  )

; finds s&p stock with largest %∆ - may take up to half an hour
(define (biggest-gain list-of-symbols)

  (if (= 1 (length list-of-symbols)) (first list-of-symbols)
      (if (> (first-price-change list-of-symbols)
             (first-price-change (rest list-of-symbols)))
          (biggest-gain (cons (first list-of-symbols) (list-tail list-of-symbols 2)))
          (biggest-gain (rest list-of-symbols))))
  
  
  )

;ditto
(define (biggest-loss list-of-symbols)

  (if (= 1 (length list-of-symbols)) (first list-of-symbols)
      (if (< (first-price-change list-of-symbols)
             (first-price-change (rest list-of-symbols)))
          (biggest-loss (cons (first list-of-symbols) (list-tail list-of-symbols 2)))
          (biggest-loss (rest list-of-symbols))))
  
  
  )

;the percent change of the first stock in a list
(define (first-price-change symbols)

  (stock-price-change (symbol->string (first symbols)))

  )

;all s&p symbols (because I don't want to deal with a .txt file or parse the internet)
(define s&p-symbols '(A
AA
AAPL
ABC
ABT
ACE
ACN
ADBE
ADI
ADM
ADP
ADSK
ADT
AEE
AEP
AES
AET
AFL
AGN
AIG
AIV
AIZ
AKAM
ALL
ALTR
ALXN
AMAT
AMD
AMGN
AMP
AMT
AMZN
AN
ANF
AON
APA
APC
APD
APH
APOL
ARG
ATI
AVB
AVP
AVY
AXP
AZO
BA
BAC
BAX
BBBY
BBT
BBY
BCR
BDX
BEAM
BEN
BHI
BIG
BIIB
BK
BLK
BLL
BMC
BMS
BMY
BRCM

BSX
BTU
BWA
BXP
C
CA
CAG
CAH
CAM
CAT
CB
CBG
CBS
CCE
CCI
CCL
CELG
CERN
CF
CFN
CHK
CHRW
CI
CINF
CL
CLF
CLX
CMA
CMCSA
CME
CMG
CMI
CMS
CNP
CNX
COF
COG
COH
COL
COP
COST
COV
CPB
CRM
CSC
CSCO
CSX
CTAS
CTL
CTSH
CTXS
CVC
CVH
CVS
CVX
D
DD
DE
DELL
DF
DFS
DG
DGX
DHI
DHR
DIS
DISCA
DLTR
DNB
DNR
DO
DOV
DOW
DPS
DRI
DTE
DTV
DUK
DVA
DVN
EA
EBAY
ECL
ED
EFX
EIX
EL
EMC
EMN
EMR
EOG
EQR
EQT
ESRX
ESV
ETFC
ETN
ETR
EW
EXC
EXPD
EXPE
F
FAST
FCX
FDO
FDX
FE
FFIV
FHN
FII
FIS
FISV
FITB
FLIR
FLR
FLS
FMC
FOSL
FRX
FSLR
FTI
FTR
GAS
GCI
GD
GE
GILD
GIS
GLW
GME
GNW
GOOG
GPC
GPS
GS
GT
GWW
HAL
HAR
HAS
HBAN
HCBK
HCN
HCP
HD
HES
HIG
HNZ
HOG
HON
HOT
HP
HPQ
HRB
HRL
HRS
HSP
HST
HSY
HUM
IBM
ICE
IFF
IGT
INTC
INTU
IP
IPG
IR
IRM
ISRG
ITW
IVZ
JBL
JCI
JCP
JDSU
JEC
JNJ
JNPR
JOY
JPM
JWN
K
KEY
KIM
KLAC
KMB
KMI
KMX
KO
KR
KRFT
KSS
L
LEG
LEN
LH
LIFE
LLL
LLTC
LLY
LM
LMT
LNC
LO
LOW
LRCX
LSI
LTD
LUK
LUV
LYB
M
MA
MAR
MAS
MAT
MCD
MCHP
MCK
MCO
MDLZ
MDT
MET
MHP
MJN
MKC
MMC
MMM
MNST
MO
MOLX
MON
MOS
MPC
MRK
MRO
MS
MSFT
MSI
MTB
MU
MUR
MWV
MYL
NBL
NBR
NDAQ
NE
NEE
NEM
NFLX
NFX
NI
NKE
NOC
NOV
NRG
NSC
NTAP
NTRS
NU
NUE
NVDA
NWL
NWSA
NYX
OI
OKE
OMC
ORCL
ORLY
OXY
PAYX
PBCT
PBI
PCAR
PCG
PCL
PCLN
PCP
PCS
PDCO
PEG
PEP
PETM
PFE
PFG
PG
PGR
PH
PHM
PKI
PLD
PLL
PM
PNC
PNR
PNW
POM
PPG
PPL
PRGO
PRU
PSA
PSX
PWR
PX
PXD
QCOM
QEP
R
RAI
RDC
RF
RHI
RHT
RL
ROK
ROP
ROST
RRC
RRD
RSG
RTN
S
SAI
SBUX
SCG
SCHW
SE
SEE
SHW
SIAL
SJM
SLB
SLM
SNA
SNDK
SNI
SO
SPG
SPLS
SRCL
SRE
STI
STJ
STT
STX
STZ
SWK
SWN
SWY
SYK
SYMC
SYY
T
TAP
TDC
TE
TEG
TEL
TER
TGT
THC
TIE
TIF
TJX
TMK
TMO
TRIP
TROW
TRV
TSN
TSO
TSS
TWC
TWX
TXN
TXT
TYC
UNH
UNM
UNP
UPS
URBN
USB
UTX
V
VAR
VFC
VIAB
VLO
VMC
VNO
VRSN
VTR
VZ
WAG
WAT
WDC
WEC
WFC
WFM
WHR
WIN
WLP
WM
WMB
WMT
WPI
WPO
WPX
WU
WY
WYN
WYNN
X
XEL
XL
XLNX
XOM
XRAY
XRX
XYL
YHOO
YUM
ZION
ZMH))






