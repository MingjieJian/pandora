      subroutine EYLAU
     $(L,LIST,MYX)
C
C     Rudolf Loeser, 1991 Aug 08
C---- Marks the items in LIST that are to appear in the dI/dh plot.
C     There should not be more than 26 such items.
C     (This is version 2 of EYLAU.)
C     !DASH
      save
C     !DASH
      integer I, IN, J, JN, L, LIST, M, MYX
C     !DASH
      external HI, BYE
C
C               LIST(L), MYX(NW)
      dimension LIST(*), MYX(*)
C
      call HI ('EYLAU')
C     !BEG
      LIST(1) = -LIST(1)
      if(L.gt.1) then
C
        M = 1
        do 101 I = 2,L
          IN = LIST(I)
          do 100 J = 1,(I-1)
            if(LIST(J).lt.0) then
              JN = -LIST(J)
              if(MYX(IN).eq.MYX(JN)) go to 101
            end if
  100     continue
          LIST(I) = -LIST(I)
          M = M+1
  101   continue
C
        if(M.gt.26) then
  102     continue
            J = +1
            do 103 I = 2,L
              if(LIST(I).lt.0) then
                J = -J
                if(J.gt.0) then
                  LIST(I) = -LIST(I)
                  M = M-1
                end if
              end if
  103       continue
          if(M.gt.26) go to 102
        end if
C
      end if
C     !END
      call BYE ('EYLAU')
C
      return
      end
