module LordOfTheRings.Location.Cards.BanksOfSirannon (banksOfSirannon) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Tentacle))

newtype BanksOfSirannon = BanksOfSirannon LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

banksOfSirannon :: LocationCard BanksOfSirannon
banksOfSirannon = location BanksOfSirannon Cards.banksOfSirannon

-- | While Banks of Sirannon is in the staging area, each Tentacle enemy gets +2 [Defense].
instance HasModifiersFor BanksOfSirannon where
  getModifiersFor (BanksOfSirannon a) = when (a.placement == StagingArea) do
    tentacles <- select (EnemyWithTrait Tentacle)
    for_ tentacles \t -> modified a t [DefenseModifier 2]

-- | Shadow: For each point of damage dealt by this attack, the defending player must
-- discard a random card from his hand.
instance RunMessage BanksOfSirannon where
  runMessage msg l@(BanksOfSirannon a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attackModifier a eid (AttackDamageDiscardsRandomFrom pid)
      pure l
    _ -> BanksOfSirannon <$> runMessage msg a
