      subroutine FELLA
     $(XLM,V,VXS,H1,CON,COMU,SRT1,SRT2,XCOMX,XMCOA,ARG,PFCO,IDEPTH,WNL,
     $ XFX)
C     Rudolf Loeser, 1989 Feb 27
C---- Dumps, for BELLA.
C     (This is version 2 of FELLA.)
C     !DASH
      save
C     !DASH
      real*8 ARG, COMU, CON, H1, PFCO, SRT1, SRT2, V, VXS, WNL, XCOMX,
     $       XFX, XLM, XMCOA
      integer IDEPTH, J, LUEO
C     !COM
C---- MOLONGA     as of 2003 Dec 02
      integer     I,JUD,L,KOD,M
      real*8      T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,ET,SUM
      logical     DPL,DMP,STT
C
      dimension   C1(2),C3(2),C4(2),WLP(2),WLM(2)
      common      /MOLONG1/ T,H,C1,C2,C3,C4,AX,SHFT,WLP,WLM,ABISO,
     $                      ET,SUM
      common      /MOLONG2/ I,JUD,L,KOD,M
      common      /MOLONG3/ DPL,DMP,STT
C     Intermediates for CO-opacity calculation.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external HI, BYE
C     !EJECT
C
      call HI ('FELLA')
C     !BEG
      write (LUEO,100) XLM,IDEPTH,(WLM(J),WNL,WLP(J),J=1,2),
     $                 T,V,CON,H1,VXS,COMU,SHFT,XFX,XCOMX,XMCOA,
     $                 SRT1,SRT2,C3(1),C3(2),ARG,PFCO,H
  100 format(' ','Details of CO-lines opacity calculation ',
     $           'for wavelength =',1PE20.12,', at depth #',I3///
     $       ' ','WLM(12)=',E20.12,5X,'WNL=',E20.12,5X,
     $           'WLP(12)=',E20.12/
     $       ' ','WLM(13)=',E20.12,5X,'WNL=',E20.12,5X,
     $           'WLP(13)=',E20.12//
     $       ' ','TE=',E16.8,5X,'V=',E16.8,5X,'NCO=',E16.8,5X,
     $           'H1=',E16.8/
     $       ' ','VXS=',E16.6,5X,'CO-mu=',E16.8,5X,'SHFT=',E16.8,5X,
     $           'TE-term=',E16.8//
     $       ' ','XCOMX (Xmax)=',E12.4,5X,'XMCOA=',E12.4,10X,
     $           'SRT(12)=',E16.8,5X,'SRT(13)=',E16.8/
     $       ' ','A(Voigt)(12)=',E16.8,5X,'A(Voigt)(13)=',E16.8/
     $       ' ','poly(for QCO)=',E16.8,5X,'QCO(TE)=',E16.8,5X,
     $           'H=',E16.8)
C     !END
      call BYE ('FELLA')
C
      return
      end
