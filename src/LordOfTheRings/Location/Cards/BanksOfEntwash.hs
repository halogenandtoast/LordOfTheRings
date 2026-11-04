module LordOfTheRings.Location.Cards.BanksOfEntwash (banksOfEntwash) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)

newtype BanksOfEntwash = BanksOfEntwash LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

banksOfEntwash :: LocationCard BanksOfEntwash
banksOfEntwash = location BanksOfEntwash Cards.banksOfEntwash

-- | Forced: After any amount of progress is placed on Banks of Entwash, increase the
-- pursuit value by 1.
instance HasAbilities BanksOfEntwash where
  getAbilities (BanksOfEntwash a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

{- | Shadow: Either increase the pursuit value by 1, or this attack is considered
undefended. The defending player chooses; an attack already undefended leaves him only
the pursuit, as an option must be done in full (FAQ 1.44).
-}
instance RunMessage BanksOfEntwash where
  runMessage msg l@(BanksOfEntwash a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      increasePursuit 1
      pure l
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      chooseOrRunOneM pid do
        questionLabeled "banksOfEntwash.choose"
        causedBy (CardIdTarget cid)
        labeled "banksOfEntwash.pursuit" $ increasePursuit 1
        when (notNull defenders) $ labeled "banksOfEntwash.undefended" $ doStep 1 msg
      pure l
    -- the attack made undefended as the choice resolves, not as it is offered
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      makeUndefended pid eid
      pure l
    _ -> BanksOfEntwash <$> runMessage msg a
