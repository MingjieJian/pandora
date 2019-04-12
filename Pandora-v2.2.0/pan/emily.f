      subroutine EMILY
     $(NAME,LU,KODE)
C
C     Rudolf Loeser, 1981 May 18
C     SGK, May 20 2014 
C---- EMILY checks whether "NAME" is a valid element symbol. Returns
C     with KODE=1 if yes, KODE=0 if no. If no, and if LU .gt. 0, then
C     an error message is written to file LU.
C     (This is version 3 of EMILY.)
C     check also against an extra list (allows O2 and O3)
C     !DASH
      save
C     !DASH
      integer I, J, KODE, LU, N
      character BLANK*1, NAME*(*)
C     !COM
C---- ELEGANT     as of 2002 Mar 26
      integer     LNAM,LABR,NELE
      character   ENAMES*12,EABBRS*2
      dimension   LNAM(92),LABR(92),ENAMES(92),EABBRS(92)
      common      /ELEGNT1/ NELE,LNAM,LABR
      common      /ELEGNT2/ ENAMES,EABBRS
C     "ENAMES" is a list of element names, and "LNAM" specifies
C     the number of characters in each name.
C     "EABBRS" is the corresponding list of element symbols, and
C     "LABR" specifies the number of characters in each symbol.
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, HI, BYE
C     !EJECT
C
      parameter (nextra=2)
      character*2 xtraabbrs(nextra)
      data  xtraabbrs/'O2','O3'/
C
      call HI ('EMILY')
C     !BEG
      KODE = 0
C
      N = 2
      if(NAME(2:2).eq.BLANK) then
        N = 1
      end if
C
      do 100 I = 1,NELE
        J = LABR(I)
        if(N.le.J) then
          if(NAME(1:J).eq.EABBRS(I)(1:J)) then
            KODE = 1
            goto 102
          end if
        end if
  100 continue
C
C hard wire O2 and O3
      do i=1, nextra
        if (NAME(1:2).eq.xtraabbrs(I)) then
          call LINER (1,LU)
          write (LU,9101) NAME
 9101     format(' ','Warning [',A10,
     &         '] is an "extra" element symbol (EMILY).')
          KODE = 1
          goto 102
        endif 
      enddo
c
      if(LU.gt.0) then
        call LINER (1,LU)
        write (LU,9102) NAME,EABBRS
 9102   format(' ','[',A10,'] is not a valid element symbol.',
     $             ' Valid symbols are:'/
     $        (' ',20A4))
      end if
C
  102 continue
C     !END
      call BYE ('EMILY')
C
      return
      end
