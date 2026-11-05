module LordOfTheRings.Treachery.Cards.DauntingPower (dauntingPower) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
newtype DauntingPower = DauntingPower TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dauntingPower :: TreacheryCard DauntingPower
dauntingPower = treachery DauntingPower Cards.dauntingPower

instance RunMessage DauntingPower where
  runMessage msg t@(DauntingPower a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      roundModifier a GameTarget (EachCharacter CannotAttack)
      engaged <- selectAny (EnemyEngagedWith AnyPlayer)
      unless engaged $ eachPlayer \pid -> encounterRaisesThreat pid 4
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \defender -> roundModifier a defender CannotReady
      pure t
    _ -> DauntingPower <$> runMessage msg a
