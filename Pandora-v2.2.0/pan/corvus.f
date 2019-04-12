      subroutine CORVUS
     $(N,NL,NO,KILROY,FCJ,CIJ)
C
C     Rudolf Loeser, 1984 Jul 20
C---- Combines CIJ and FCJ, and prints comparisons.
C     !DASH
      save
C     !DASH
      real*8 CIJ, FCJ
      integer I, IC, J, JREF, N, NL, NO
      logical KILROY, PRNT
C     !DASH
      external INDXIJ, ABJECT, LINER, HI, BYE
C
C               FCJ(N,NL), CIJ(N,NL**2)
      dimension FCJ(N,*),  CIJ(N,*)
C
      call HI ('CORVUS')
C     !BEG
      PRNT = NO.gt.0
C
      do 103 J = 2,NL
        call INDXIJ         (1, J, IC)
        do 102 I = 1,N
          if(FCJ(I,J).gt.CIJ(I,IC)) then
C
            if(PRNT) then
              if(KILROY) then
                KILROY = .false.
                call ABJECT (NO)
                write (NO,100)
  100           format(' ','Fast electrons: the following regular ',
     $                     'values of C1J were replaced by FC1J:')
                JREF = 0
              end if
              if(JREF.ne.J) then
                JREF = J
                call LINER  (1, NO)
              end if
              write (NO,101) J,I,CIJ(I,IC),FCJ(I,J)
  101         format(' ','Level J =',I3,5X,'Depth =',I3,5X,
     $                   'C1J =',1PE16.8,5X,'FC1J =',E16.8)
            end if
C
            CIJ(I,IC) = FCJ(I,J)
          end if
  102   continue
  103 continue
C     !END
      call BYE ('CORVUS')
C
      return
      end
