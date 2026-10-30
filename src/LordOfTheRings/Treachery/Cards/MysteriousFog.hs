module LordOfTheRings.Treachery.Cards.MysteriousFog (mysteriousFog) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype MysteriousFog = MysteriousFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mysteriousFog :: TreacheryCard MysteriousFog
mysteriousFog = treachery MysteriousFog Cards.mysteriousFog

-- | When Revealed: Either each player must exhaust a character he controls, or each Lost
-- Island in the staging area gets +1 threat until the end of the phase.
-- Shadow: Discard the bottom card of your deck. If that card's printed cost is 2 or less,
-- exhaust a character you control.
instance RunMessage MysteriousFog where
  runMessage msg t@(MysteriousFog attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      islands <- select (LocationInStagingArea <> unchartedLocation)
      chooseOneM lead do
        questionLabeled "mysteriousFog.choose"
        causedBy attrs
        labeled "mysteriousFog.exhaust" $ eachPlayer \p -> chooseAndExhaust attrs p 1
        labeled "mysteriousFog.threat" $ for_ islands \l -> phaseModifier attrs l (ThreatModifier 1)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      bottom <- discardBottomOfDeck pid
      when (maybe False ((<= 2) . printedCostOf) bottom) $ chooseAndExhaust (CardIdSource cid) pid 1
      pure t
    _ -> MysteriousFog <$> runMessage msg attrs
