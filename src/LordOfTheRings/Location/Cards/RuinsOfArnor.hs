module LordOfTheRings.Location.Cards.RuinsOfArnor (ruinsOfArnor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RuinsOfArnor = RuinsOfArnor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinsOfArnor :: LocationCard RuinsOfArnor
ruinsOfArnor = location RuinsOfArnor Cards.ruinsOfArnor

-- | Forced: After the "when revealed" effect of a treachery card is canceled, raise each
-- player's threat by 2.
instance HasAbilities RuinsOfArnor where
  getAbilities (RuinsOfArnor a) = [mkAbility a 1 $ forced $ TreacheryWhenRevealedCanceledWindow After]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, the
-- defending player raises his threat by 2.
instance RunMessage RuinsOfArnor where
  runMessage msg l@(RuinsOfArnor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 2
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      encounterRaisesThreat pid 2
      pure l
    _ -> RuinsOfArnor <$> runMessage msg a
