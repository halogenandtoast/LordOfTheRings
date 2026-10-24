module LordOfTheRings.Event.Cards.MendorsSupport (mendorsSupport) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Card.CardDef (cdCardCode)
import LordOfTheRings.Event.Import.Lifted

newtype MendorsSupport = MendorsSupport EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Only one copy of Mendor's Support can be played each round.
-- Action: Mendor gets +2 willpower, +2 attack, and +2 defense until the end of the round.
mendorsSupport :: EventCard MendorsSupport
mendorsSupport =
  eventWith MendorsSupport Cards.mendorsSupport \a ->
    a
      & criteriaL
      .~ Criteria
        [ exists (AllyWithTitle "Mendor")
        , TargetLacksModifier GameTarget (PlayedThisRound $ cdCardCode Cards.mendorsSupport)
        ]

instance RunMessage MendorsSupport where
  runMessage msg e@(MendorsSupport attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      roundModifier attrs GameTarget (PlayedThisRound $ cdCardCode Cards.mendorsSupport)
      mendors <- select (AllyWithTitle "Mendor")
      for_ mendors \m -> roundModifiers attrs m [WillpowerModifier 2, AttackModifier 2, DefenseModifier 2]
      pure e
    _ -> MendorsSupport <$> runMessage msg attrs
