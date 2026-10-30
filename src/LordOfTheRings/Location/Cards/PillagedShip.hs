module LordOfTheRings.Location.Cards.PillagedShip (pillagedShip) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype PillagedShip = PillagedShip LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pillagedShip :: LocationCard PillagedShip
pillagedShip = location PillagedShip Cards.pillagedShip

-- | When Revealed: Either deal 3 damage to Pillaged Ship, or add the topmost enemy in the
-- encounter discard pile to the staging area. The first player chooses.
-- Shadow: If attacking enemy is a Raider, move all resources from defending character to
-- attacking enemy.
instance RunMessage PillagedShip where
  runMessage msg l@(PillagedShip a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      discarded <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "pillagedShip.choose"
        causedBy a
        labeled "pillagedShip.damage" $ dealDamage a a 3
        for_ (find ((== EnemyType) . cdCardType . toCardDef) discarded) \enemy ->
          labeled "pillagedShip.enemy" $ addToStagingFromDiscard enemy
      pure l
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) do
        for_ [hid | HeroTarget hid <- defenders] \hid -> do
          n <- (.resources) <$> getHero hid
          when (n > 0) do
            removeTokens (CardIdSource cid) hid Resource n
            placeResources (CardIdSource cid) eid n
      pure l
    _ -> PillagedShip <$> runMessage msg a
