module LordOfTheRings.Ally.Cards.Coruhaer (coruhaer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype Coruhaer = Coruhaer AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Loot the Dungeons.
coruhaer :: AllyCard Coruhaer
coruhaer = ally Coruhaer Cards.coruhaer

-- | Response: After Coruhaer defends an attack, spend 1 resource to deal 2 damage to the
-- attacking enemy.
instance HasAbilities Coruhaer where
  getAbilities (Coruhaer a) = [mkAbility a 1 $ ResponseAbility (defendedAttack a) (SpendResourcesCost Neutral 1)]

instance RunMessage Coruhaer where
  runMessage msg s@(Coruhaer a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [eid | Window _ (DefendedAttack _ eid) <- windows] \eid -> dealDamage a eid 2
      pure s
    _ -> Coruhaer <$> runMessage msg a
