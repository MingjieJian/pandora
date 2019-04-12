      subroutine TIMOTHY
     $(DUMP,IU,IL,XNE,FSTKM,MAX,M,DWN,CDL,ZND)
C
C     Rudolf Loeser, 1992 Jan 22
C---- Produces the complete initial raw set of DWN and CDL, each of
C     length M (M .le. MAX), for the (u,l) transition of Hydrogen and
C     the specified value of electron density.
C     (This is version 3 of TIMOTHY.)
C     !DASH
      save
C     !DASH
      real*8 CDL, CONST, DD, DWN, F3, FL, FOUR, FSTKM, FU, TWO, XNE,
     $       XPN, ZND
      integer I, IL, IU, J, LUEO, M, MAX
      logical DUMP
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- TRETA       as of 1992 Jan 24
      integer     MU,ML,N1U,N1L,N2U,N2L
      real*8      FMU,FML,FN1U,FN1L,FN2U,FN2L,FUL
      dimension   FUL(7)
      common      /TRETA1/ MU,ML,N1U,N1L,N2U,N2L
      common      /TRETA2/ FMU,FML,FN1U,FN1L,FN2U,FN2L,FUL
C     Parameters for TIMOTHY and its callees.
C     .
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 5),FOUR  )
C
C     !DASH
C     !EJECT
      external  HALT, TIMBER, TOBIAS, BONITO, LINER, HI, BYE
      intrinsic abs
C
C               DWN(MAX), CDL(MAX), ZND(MAX)
      dimension DWN(*),   CDL(*),   ZND(*)
C
      data CONST,XPN,F3 /8.11D-11, 6.6666666666666667D-1, 1.D3/
C
      call HI ('TIMOTHY')
C     !BEG
      DD = CONST*FSTKM*(XNE**XPN)
      FU = IU
      FL = IL
      FUL(1) = (-FOUR*FU*FL)/((FU-FL)**2)
      FUL(2) = ((FU**2)+(FL**2))/((FU+FL)**2)
      FUL(3) = ((FU-FL)/(FU+FL))**2
      FUL(7) = (FOUR*FU*FL)/((FU+FL)**2)
C
      M = 0
C
      do 106 MU = 0,(IU-1)
        FMU = MU
        FUL(4) = FUL(1)**(2*MU)
        FUL(5) = FOUR*FUL(4)
        I = IU-1-MU
C
        do 105 ML = 0,(IL-1)
          if(abs(ML-MU).le.1) then
            FML = ML
            FUL(6) = FOUR*(FUL(1)**(2*ML))
            J = IL-1-ML
            if(I.ge.0) then
C
              do 104 N1U = 0,I
                FN1U = N1U
                if(J.ge.0) then
C
                  do 103 N1L = 0,J
                    FN1L = N1L
C
                    N2U  = I-N1U
                    FN2U = N2U
                    N2L  = J-N1L
                    FN2L = N2L
                    if((N2U.ge.0).and.(N2L.ge.0)) then
C
                      M = M+1
C
                      if(M.gt.MAX) then
                        write (MSSLIN(1),100) M,MAX
  100                   format('M =',I12,', which is greater than the ',
     $                         'allowed MAX =',I12,'.')
                        call HALT ('TIMOTHY', 1)
                      end if
C     !EJECT
                      if(DUMP) then
                        call LINER (1,LUEO)
                        write (LUEO,101) IU,IL,MU,ML,N1U,N1L,N2U,N2L,
     $                                 XNE,DD,FUL
  101                   format(' ','***** IU=',I3,'  IL=',I3,'  MU=',
     $                             I3,'  ML=',I3,'  N1U=',I3,'  N1L=',
     $                             I3,'  N2U=',I3,'  N2L=',I3/
     $                         ' ','***** NE=',1PE16.8,'  DEL=',E16.8/
     $                         ' ','***** FUL=',7E16.8/
     $                         ' ','*****')
                      end if
C
                      DWN(M) = DD*(FU*(FN1U-FN2U)-FL*(FN1L-FN2L))
                      if(ML.le.(MU-1)) then
                        call TIMBER (DUMP, CDL(M))
                      else if(ML.ge.(MU+1)) then
                        call TOBIAS (DUMP, CDL(M))
                      else
                        call BONITO (DUMP, CDL(M))
                        if(MU.ne.0) then
                          CDL(M) = TWO*CDL(M)
                        end if
                      end if
C
                      ZND(M) = F3*(F3*(F3*FMU+FML)+FN1U)+FN1L
C
                      if(DUMP) then
                        write (LUEO,102) M,DWN(M),CDL(M),ZND(M)
  102                   format(' ','**********'/
     $                         ' ','**********',I5,'  DWN=',1PE16.8,
     $                             '  CDL=',E16.8,5X,'ZDN=',0PE22.12)
                      end if
C
                    end if
  103             continue
C                 End-of-loop: N1L
C
                end if
  104         continue
C             End-of-loop: N1U
C
            end if
          end if
  105   continue
C       End-of-loop: ML
C
  106 continue
C     End-of-loop: MU
C
C     !END
      call BYE ('TIMOTHY')
C
      return
      end
