      subroutine PHAEDRA
     $(N,AO,AF,EDIT)
C
C     Rudolf Loeser, 1989 Jul 18
C---- Edits an original table, AO, so that the final table, AF,
C     contains no values less than zero.
C     (This is version 2 of PHAEDRA.)
C     !DASH
      save
C     !DASH
      real*8 AF, AO, ZERO
      integer I, N
      logical EDIT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               AO(N), AF(N)
      dimension AO(*), AF(*)
C
      call HI ('PHAEDRA')
C     !BEG
      EDIT = .false.
C
      if(N.gt.0) then
        do 100 I = 1,N
          if(AO(I).lt.ZERO) then
            EDIT  = .true.
            AF(I) = ZERO
          else
            AF(I) = AO(I)
          end if
  100   continue
      end if
C     !END
      call BYE ('PHAEDRA')
C
      return
      end
