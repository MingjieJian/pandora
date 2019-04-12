      subroutine UNTANA
     $(N,K,XI,DL,TNUSAV,KISSAV,KILSAV)
C
C     Rudolf Loeser, 2000 Feb 08
C---- Prints a TNU-analysis.
C     !DASH
      save
C     !DASH
      real*8 DL, TNUSAV, VAL, XI
      integer I, IL, IU, J, JE, JS, K, KILSAV, KISSAV, L, M, MO, N
      character BLANK*1, MARK*1, STAR*1
C     !COM
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
C     !EJECT
      external  LINER, VECOUT, SHIM, HI, BYE
      intrinsic min
C
C               TNUSAV(N,KM), KISSAV(KM), KILSAV(KM), XI(KM), DL(KM)
      dimension TNUSAV(N,*),  KISSAV(*),  KILSAV(*),  XI(*),  DL(*)
C
      dimension VAL(15), MARK(15)
C
      call HI ('UNTANA')
C     !BEG
      if(MO.gt.0) then
        call LINER    (5, MO)
        write (MO,100) IU,IL
  100   format(' ','TNU Analysis: log10(TNU) at all frequencies, ',
     $             'for transition ',I2,'/',I2,'.',39X,
     $             '(controlled by KANTNU)')
        call VECOUT   (MO, XI, K, 'Values of frequency (XI)')
        call VECOUT   (MO, DL, K, 'Values of delta-lambda (DL)')
C
        JE = 0
  101   continue
C
          JS = JE+1
          JE = min((JE+15),K)
          call LINER  (2, MO)
          write (MO,102) (J,J=JS,JE)
  102     format(' ',4X,15I8)
          call LINER  (1, MO)
C
          do 105 I = 2,N
            M = 0
C
            do 103 J = JS,JE
              M = M+1
              VAL(M)  = log10(TNUSAV(I,J))
              MARK(M) = BLANK
              if((KISSAV(J).eq.I).or.(KILSAV(J).eq.I)) then
                MARK(M) = STAR
              end if
  103       continue
C
            write (MO,104) I,(VAL(L),MARK(L),L=1,M)
  104       format(' ',I5,15(F7.2,A1))
            call SHIM (I, 5, MO)
C
  105     continue
C
        if(JE.lt.K) goto 101
C
      end if
C     !END
      call BYE ('UNTANA')
C
      return
      end
