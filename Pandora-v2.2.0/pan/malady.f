      subroutine MALADY
     $(NMLR,N,MVEL,VEL,RVEL,VREQ,HREQ,Z,TE,FRS,HND,NO)
C
C     Rudolf Loeser, 2005 Sep 14
C---- Prints mass loss rates data.
C     !DASH
      save
C     !DASH
      real*8 FAC, FRS, HND, HREQ, RVEL, SG, TE, VEL, VREQ, Z
      integer I, J, JS, MVEL, N, NMLR, NO
      logical ZV
C     !DASH
      external ABJECT, NAUGHTD, LINER, SHIM, HI, BYE
C
C               VEL(N,MVEL), RVEL(N,MVEL), VREQ(N,MVEL), HREQ(N,MVEL),
      dimension VEL(N,*),    RVEL(N,*),    VREQ(N,*),    HREQ(N,*),
C
C               FRS(N), HND(N), TE(N), Z(N)
     $          FRS(*), HND(*), TE(*), Z(*)
C
      data FAC /1.587D-26/
C
      call HI ('MALADY')
C     !BEG
      if(NO.gt.0) then
        call ABJECT  (NO)
        write (NO,100) NMLR,NMLR
  100   format(' ','Summary of mass flux associated with each ',
     $             'velocity in the above set.'//
     $         ' ','TE is temperature (K), Z is geometrical depth ',
     $             '(km, positive inwards), and DR is distance ',
     $             '(radii).'/
     $         ' ','MLR is mass flux (g/s), and MLR(solar) is mass ',
     $             'flux (solar masses /year).'/
     $         ' ','V is velocity (km/s, positive outwards).'/
     $         ' ','V* is the velocity required for a constant mass ',
     $             'flux equal to MLR at depth index NMLR =',I5/
     $         ' ','HND is the total hydrogen density (/cm**3).'/
     $         ' ','HND* is the total hydrogen density required for ',
     $             'a constant mass flux equal to MLR at depth ',
     $             'index NMLR =',I5)
C
        JS = 1
        call NAUGHTD (VEL(1,1), 1, N, ZV)
        if(ZV) then
          JS = 2
        end if
C     !EJECT
        if(JS.le.MVEL) then
          do 104 J = JS,MVEL
            call LINER  (2, NO)
            write (NO,101) J
  101       format(' ','Velocity Set #',I3//
     $             ' ',16X,'TE',12X,'Z',11X,'DR',10X,'MLR',3X,
     $                 'MLR(solar)',12X,'V',11X,'V*',10X,'HND',9X,
     $                 'HND*')
            call LINER  (1, NO)
C
            do 103 I = 1,N
              SG = RVEL(I,J)*FAC
              write (NO,102) I,TE(I),Z(I),FRS(I),RVEL(I,J),SG,
     $                       VEL(I,J),VREQ(I,J),HND(I),HREQ(I,J)
  102         format(' ',I5,1P9E13.5)
              call SHIM (I, 5, NO)
  103       continue
C
  104     continue
        end if
      end if
C     !END
      call BYE ('MALADY')
C
      return
      end
