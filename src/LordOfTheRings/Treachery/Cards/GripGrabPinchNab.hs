module LordOfTheRings.Treachery.Cards.GripGrabPinchNab (gripGrabPinchNab) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (cdCardTraits, cdCardType), CardType (EnemyType), toCardDef)
import LordOfTheRings.Trait (Trait (Goblin))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GripGrabPinchNab = GripGrabPinchNab TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gripGrabPinchNab :: TreacheryCard GripGrabPinchNab
gripGrabPinchNab = treachery GripGrabPinchNab Cards.gripGrabPinchNab

-- | When Revealed: Starting with the first player, each player must choose 1 Goblin enemy
-- from the discard pile and add it to the staging area: one after another, each choosing
-- from what the one before left.
-- Shadow: Defending player deals damage among characters he controls equal to the number
-- of Goblin enemies engaged with him.
instance RunMessage GripGrabPinchNab where
  runMessage msg t@(GripGrabPinchNab attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure t
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (atMay players (n - 1)) \p -> do
        goblins <- filter goblinEnemy <$> getEncounterDiscard
        chooseOrRunOneM p do
          questionLabeled "gripGrabPinchNab.choose"
          causedBy attrs
          showing (map EncounterCard goblins)
          for_ goblins \c -> targeting (CardIdTarget c.id) (addToStagingFromDiscard c)
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      goblins <- length <$> select (EnemyWithTrait Goblin <> EnemyEngagedWith (PlayerWithId pid))
      when (goblins > 0) $ dealDamageDivided attrs pid goblins
      pure t
    _ -> GripGrabPinchNab <$> runMessage msg attrs
   where
    goblinEnemy c = Goblin `member` cdCardTraits (toCardDef c) && cdCardType (toCardDef c) == EnemyType
