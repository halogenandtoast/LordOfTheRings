module LordOfTheRings.Treachery.Cards.MidnightThrong (midnightThrong) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)
import LordOfTheRings.Trait (Trait (Undead))

newtype MidnightThrong = MidnightThrong TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

midnightThrong :: TreacheryCard MidnightThrong
midnightThrong = treachery MidnightThrong Cards.midnightThrong

instance HasModifiersFor MidnightThrong where
  getModifiersFor (MidnightThrong a) = do
    time <- currentNight
    when (time == Just Midnight) $ modifySelf a [WhenRevealedCannotBeCanceled]

instance RunMessage MidnightThrong where
  runMessage msg t@(MidnightThrong a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> searchEncounterDeckAndDiscardToReveal pid a (CardWithType EnemyType <> CardWithTrait Undead)
      shuffleEncounterDeck
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      time <- currentNight
      let bonus = case time of Just Midnight -> 3; Just Dusk -> 2; _ -> 1
      enemyAttackModifier a eid (AttackModifier bonus)
      pure t
    _ -> MidnightThrong <$> runMessage msg a
