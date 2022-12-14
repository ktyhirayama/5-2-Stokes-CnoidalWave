C###########################################################
C
C  (c) 1999  by Masahiko Isobe
C
C    Programmed by 
C       Masahiko Isobe,     
C       Department of Civil Engneering, 
C       University of Tokyo, 
C       Hongo 7-3-1, Bunkyo-ku, Tokyo 113-8656, Japan
C    Slightly modified by 
C       Xiping Yu, 
C       Department of Engneering Mechanics,
C       Shanghai Jiao Tong University,  
C       1954 Hua Shan Road, Shanghai 200030, China
C
C   This program is provided as is. The authors are not 
C   responsible regarding the use or results of use of
C   this program. The program is free for educational  
C   purposes. For other kind of use, permission by 
C   the first author is necessary.
C
C###########################################################
C
      SUBROUTINE STK(N1,D,T,H,RL,C)
C
C------------------------------------------------
C  List of Symbols
C    N1 (i)  = (I) Order of the theory  (<=5)
C    D1 (f)  = (I) Mean water depth     (m)
C    T  (f)  = (I) Wave period          (s)
C    H  (f)  = (I) Wave height          (m)
C    RL (f)  = (O) Wavelength           (m)
C    C  (f)  = (O) Phase velocity       (m/s)
C------------------------------------------------
C
      DIMENSION A(5),B(5),SI(5),CO(5),SH(5),CH(5)
      SAVE
      DATA EPS/1.E-5/,G,PI/9.80,3.141593/
C
      X=1.E10
      Z=1.E10
      N=N1
      SGM=2.*PI/T
      WW=SGM*SGM*D/G
      W=SQRT(WW)
      HD=H/(2.*D)
      DK=HK(WW)
      IC=0
C
      DO 11 ITER=1,20
      DDK=SIGN(DK*EPS,DDDK)
      DK1=DK+DDK
   16 TH=TANH(DK1)
      CT=1./TH
      R=CT*CT
      F=(HD*DK1)**2
      C1=0.
      GO TO (12,13,13,14,14),N
   14 C1=(39.+(351.+(-2194.+(2454.+(-117.-405.*R)*R)*R)*R)*R)/1024.
     #   -(3.+(-7.+(-3.-9.*R)*R)*R)*CT/(64.*DK1)
   13 C1=C1*F+(9.+(-10.+9.*R)*R)/16.-CT/(2.*DK1)
   12 C1=C1*F+1.
      R1=SQRT(DK1*TH)*C1-W
      IF(IC.EQ.1) GO TO 20
      IF(DK1.EQ.DK) GO TO 15
      R2=R1
      DK1=DK
      GO TO 16
   15 DDDK=-R1*DDK/(R2-R1)
      DK=DK+DDDK
      IF(ABS(DDDK/DK).GE.EPS) GO TO 11
C
      IC=1
      DK1=DK
      GO TO 16
C
   11 CONTINUE
      DK=DK-DDDK
   20 RK=DK/D
      RL=2.*PI/RK
      E=DK*HD
      C0=SQRT(G*TH/RK)
      CC=C1*C0
      C=CC
      RK1=RK
      DK1=DK
      E1=E
      C01=C0
C
      E2=E*E
      E3=E2*E
      E4=E3*E
      E5=E4*E
      DO 36 I=1,N
      A(I)=0.
   36 B(I)=0.
      B0=0.
      P0=0.
C
      GO TO (31,32,33,34,35),N
   35 A(1)=E5*(-225.+(2370.+(14622.+(-6070.+(53932.+(-340410.+(-764046.+
     #         (203310.+188325.*R)*R)*R)*R)*R)*R)*R)*R)
     #          /(12288.*(1.+5.*R)*(3.+5.*R))
      A(3)=E5*(-180.+(-1008.+(900.+(-5328.+(46980.+(-7776.-14580.*R)*R)
     #         *R)*R)*R)*R)*R/(4096.*(1.+5.*R))
      A(5)=E5*(45.+(-150.+(-570.+(2618.+(-3896.+(-498.+(25866.+(8910.
     #         +6075.*R)*R)*R)*R)*R)*R)*R)*R)*5.
     #         /(12288.*(1.+5.*R)*(3.+5.*R))
      B(1)=E5*(861.+(12262.+(30114.+(-75986.+(-131460.+(-91438.
     #         +(-199602.+(89370.+62775.*R)*R)*R)*R)*R)*R)*R)*R)
     #         /(4096.*(1.+5.*R)*(3.+5.*R))
      B(3)=E5*(173.+(1413.+(1665.+(-6591.+(-7569.+(10935.+(4131.-3645.
     #         *R)*R)*R)*R)*R)*R)*R)/(1024.*(1.+5.*R))
      B(5)=E5*(1083.+(15230.+(39970.+(-78730.+(-89928.+(157690.
     #         +(-16290.+(-39150.+10125.*R)*R)*R)*R)*R)*R)*R)*R)
     #         /(4096.*(1.+5.*R)*(3.+5.*R))
   34 A(2)=E4*(12.+(-352.+(792.-324.*R*R)*R)*R)*CT/384.
      A(4)=E4*(21.+(1.+(-262.+(522.+(81.+405.*R)*R)*R)*R)*R)*CT
     #         /(384.*(1.+5.*R))
      B(2)=E4*(-103.+(19.+(261.+(81.-162.*R)*R)*R)*R)*CT/192.
      B(4)=E4*(-197.+(-747.+(1466.+(342.+(-1269.+405.*R)*R)*R)*R)*R)*CT
     #         /(384.*(1.+5.*R))
      B0  =E4*(-3.+(7.+(3.+9.*R)*R)*R)*CT/(64.*DK)
      P0  =E4*((-1.+R)*(21.+(13.+(-57.-9.*R)*R)*R)/128.
     #         -3.*CT*(-1.+R)*(7.+(-2.+3.*R)*R)/(64.*DK)-R/(8.*DK*DK))
   33 A(1)=A(1)+E3*(1.+(-3.+(3.-9.*R)*R)*R)*3./64.
      A(3)=A(3)+E3*(-1.+(3.+(-3.+9.*R)*R)*R)*3./64.
      B(1)=B(1)+E3*(39.+(-41.+(-3.-27.*R)*R)*R)/64.
      B(3)=B(3)+E3*(13.+(17.+(-57.+27.*R)*R)*R)/64.
   32 A(2)=A(2)+E2*(-1.+3.*R)*CT/4.
      B(2)=B(2)+E2*(-1.+R)*CT*3./4.
      B0  =B0  +E2*(-CT/(2.*DK))
      P0  =P0  +E2*((-1.+R)/4.+CT/(2.*DK))
   31 A(1)=A(1)+E
      B(1)=B(1)+E
C
      DO 37 I=1,N
      A(I)=A(I)/RK
   37 B(I)=B(I)*C0*I/SINH(I*DK)
      B0  =B0  *C0
      P0  =P0  *C0*C0/G
      RETURN
C
C
      ENTRY STKETA(X1,ZS)
C
C------------------------------------------------
C  List of Symbols
C    X1 (f)  = (I) Relatice phase
C                [normalized by 2Pi; 0 at crest]  
C    ZS (f)  = (O) Surface elevation     (m)
C                [0 at M.W.L.]
C------------------------------------------------
C
      ISUB=1
      IF(X.NE.X1) GO TO 210
  110 ZS=0.
      DO 111 I=1,N
  111 ZS=ZS+A(I)*CO(I)
      RETURN
C
C
      ENTRY STKUWP(X1,Z1,U1,W1,P1)
C
C------------------------------------------------
C  List of Symbols
C    X1 (f)  = (I) Relatice phase
C                [normalized by 2Pi; 0 at crest]  
C    Z1 (f)  = (I) Vertical position
C                [0 at M.W.L.]  
C    U1 (f)  = (O) Horizontal velocity   (m/s)
C    V1 (f)  = (O) Vertical velocity     (m/s)
C    P1 (f)  = (O) Pressure head         (m)  
C------------------------------------------------
C
      ISUB=2
      IF(X.EQ.X1) GO TO 220
  210 X=X1
      XK=2.*PI*X
      DO 211 I=1,N
      XI=XK*I
      CO(I)=COS(XI)
  211 SI(I)=SIN(XI)
      IF(ISUB.EQ.1) GO TO 110
C
  220 IF(Z.EQ.Z1) GO TO 230
      Z=Z1
      ZK=Z*RK+DK
      DO 221 I=1,N
      ZI=ZK*I
      CH(I)=COSH(ZI)
  221 SH(I)=SINH(ZI)
C
  230 U=B0
      W=0.
      DO 231 I=1,N
      U=U+B(I)*CH(I)*CO(I)
  231 W=W+B(I)*SH(I)*SI(I)
      U1=U
      W1=W
      P1=P0+(CC*U-(U*U+W*W)/2.)/G
      RETURN
      END
C
C
      FUNCTION HK(WW)
C
C  Solution of Y from X = Y Tanh Y
C    with Y = HK  and  X = WW  
C
      DATA EPS,NMAX/1.E-6,10/
      HK=WW
      IF(WW.GT.15.) RETURN
      HK=SQRT(WW)
      DO 10 N=1,NMAX
      T=TANH(HK)
      D=(HK*T-WW)/(T+HK*(1.-T*T))
      HK=HK-D
      IF(ABS(D/HK).LT.EPS) RETURN
   10 CONTINUE
      RETURN
      END
