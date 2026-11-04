module LordOfTheRings.Treachery.Cards.PlaceOfEvil (placeOfEvil) where

import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers (placeDamageOnTribute)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PlaceOfEvil = PlaceOfEvil TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

placeOfEvil :: TreacheryCard PlaceOfEvil
placeOfEvil = treachery PlaceOfEvil Cards.placeOfEvil

{- | When Revealed: Place 1 damage token on Tribute to Mordor. Then, either place 1
additional damage token on Tribute to Mordor, or Place of Evil gains surge. The first
player chooses.
Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, place 1
damage token on Tribute to Mordor.
-}
instance RunMessage PlaceOfEvil where
  runMessage msg t@(PlaceOfEvil attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeDamageOnTribute attrs
      lead <- getLead
      chooseOneM lead do
        questionLabeled "placeOfEvil.choose"
        causedBy attrs
        labeled "placeOfEvil.damage" $ placeDamageOnTribute attrs
        labeled "placeOfEvil.surge" revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeDamageOnTribute attrs
      pure t
    _ -> PlaceOfEvil <$> runMessage msg attrs
