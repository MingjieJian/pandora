      subroutine RONALD
     $(ARR,BRR,QLL,QAR,N,HBC,IZERO)
C
C     Rudolf Loeser, 1991 Nov 22
C---- Writes a line of CSK values or notes.
C     !DASH
      save
C     !DASH
      real*8 ARR, BRR, ZERO
      integer I, IZERO, N
      logical HBC
      character BLANK*1, QAR*10, QLL*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, RANDALL, HI, BYE
C
C               ARR(N), BRR(N), QLL(16), QAR(16)
      dimension ARR(*), BRR(*), QLL(*),  QAR(*)
C     !EJECT
C
      call HI ('RONALD')
C     !BEG
      if((N.lt.1).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,', which is not 1 through 8, inclusive.')
        call HALT  ('RONALD',1)
      end if
C
      call RANDALL (ARR,QLL,QAR,N,IZERO)
C
      if(HBC) then
        do 101 I = 2,N
          if(BRR(I).gt.ZERO) then
            QAR(I) = '  (unused)'
          end if
  101   continue
      end if
C     !END
      call BYE ('RONALD')
C
      return
      end
