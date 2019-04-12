      subroutine SHRIMP
     $(ABSCISS,N,IBEG,IEND,ORDINAT,M,TAB,NT,SIG,KODE,IMAGE)
C
C     Rudolf Loeser, 1982 May 14
C---- Generalized routine for entering two-dimensioned data
C     arrays into a graph image, such that each column of the
C     array is treated as a set.
C     KODE=1 means: plot individual data points only;
C         =2      : plot connecting line segments as well.
C     (This is version 3 of SHRIMP.)
C     !DASH
      save
C     !DASH
      real*8 ABSCISS, ORD, ORDINAT, SIG
      integer I, IBEG, IEND, J, KODE, LINC, M, N, NT
      logical SEGMENT
      character BLANK*1, IMAGE*(*), SYM*1, TAB*1
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external HALT, GARLIC, LINK, KPLOTC, HI, BYE
C
C               ABSCISS(N), ORDINAT(N,M), TAB(NT)
      dimension ABSCISS(*), ORDINAT(N,*), TAB(*)
C
      call HI ('SHRIMP')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1 or 2.')
        call HALT           ('SHRIMP', 1)
      end if
C
      SEGMENT = KODE.eq.2
C
      do 102 J = 1,M
        call GARLIC         (J, TAB, NT, SYM)
        if(SYM.ne.BLANK) then
          LINC  = 1
C
          do 101 I = IBEG,IEND
            ORD = ORDINAT(I,J)
            if(ORD.ne.SIG) then
              if(SEGMENT) then
                call LINK   (IMAGE, ABSCISS(I), ORD, SYM, LINC)
              else
                call KPLOTC (IMAGE, ABSCISS(I), ORD, SYM      )
              end if
            else
              LINC = 1
            end if
  101     continue
C
        end if
  102 continue
C     !END
      call BYE ('SHRIMP')
C
      return
      end
