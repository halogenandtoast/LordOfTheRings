module LordOfTheRings.Treachery.Cards.MurmursOfDread (murmursOfDread) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype MurmursOfDread = MurmursOfDread TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

murmursOfDread :: TreacheryCard MurmursOfDread
murmursOfDread = treachery MurmursOfDread Cards.murmursOfDread

instance RunMessage MurmursOfDread where
  runMessage msg t@(MurmursOfDread a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \target -> roundModifier a target (WillpowerModifier (-1))
      pure t
    RevealShadowCard eid cid | cid == a.cardId -> do
      time <- currentNight
      when (time == Just Midnight) $
        createEffect (CardIdSource cid) (CardIdTarget cid) [ShadowCannotBeCanceled] (UntilShadowCardLeaves eid cid)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      time <- currentNight
      when (time `elem` [Just Dusk, Just Midnight]) $ additionalAttack pid eid
      pure t
    _ -> MurmursOfDread <$> runMessage msg a
