module LordOfTheRings.Location.Cards.CrackedPillar (crackedPillar) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CrackedPillar = CrackedPillar LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

crackedPillar :: LocationCard CrackedPillar
crackedPillar = location CrackedPillar Cards.crackedPillar

instance HasModifiersFor CrackedPillar where
  getModifiersFor (CrackedPillar a) =
    when (a.damage < 4) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage CrackedPillar where
  runMessage msg l@(CrackedPillar a) = case msg of
    -- One normal attack opportunity, during the first player's attacks. Its temporary
    -- enemy actor runs the normal declaration, ranged joining and attack response windows.
    PlayerAttacks pid | a.placement == StagingArea -> do
      lead <- getLead
      phase <- getsGame (.phase)
      used <- elem DeclaredAttackerThisRound <$> getModifiers a
      when (lead == pid && phase == CombatPhase && not used) do
        barred <- elem CannotAttack <$> getModifiers (PlayerTarget pid)
        unless barred $ chooseOneM pid do
          questionLabeled "crackedPillar.attack"
          causedBy a
          targeting a $ push $ BeginLocationAttack pid a.id
          labeled "doNotAttack" $ pure ()
      pure l
    TravelTo lid | lid == a.id -> do
      removeTokens a a Damage 4
      pure l
    _ -> CrackedPillar <$> runMessage msg a
