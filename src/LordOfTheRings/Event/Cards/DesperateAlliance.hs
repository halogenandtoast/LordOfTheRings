module LordOfTheRings.Event.Cards.DesperateAlliance (desperateAlliance) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (immuneToSource)

newtype DesperateAlliance = DesperateAlliance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

{- | Action: Choose a hero you control. Until the end of the phase, give control of that hero
and all resources in that hero's resource pool to another player. (Limit 1 per phase.)

The resources stay in the hero's pool, so they go with him. The hero comes back at the end
of the phase, and his player is not eliminated meanwhile for having no hero (FAQ 1.9).
-}
desperateAlliance :: EventCard DesperateAlliance
desperateAlliance =
  eventWith DesperateAlliance Cards.desperateAlliance \a ->
    a
      & criteriaL
      .~ ( PlayerExists (NotPlayer $ PlayerWithId a.owner)
             <> TargetLacksModifier (PlayerTarget a.owner) (PlayedThisPhase $ toCardCode Cards.desperateAlliance)
         )

instance RunMessage DesperateAlliance where
  runMessage msg e@(DesperateAlliance attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      phaseModifier attrs (PlayerTarget pid) (PlayedThisPhase $ toCardCode Cards.desperateAlliance)
      -- one immune to player card effects, as Beorn, cannot be chosen
      heroes <- filterM (fmap not . immuneToSource (toSource attrs)) =<< select (HeroControlledBy $ PlayerWithId pid)
      others <- filter (/= pid) <$> getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "desperateAlliance.chooseHero"
        causedBy attrs
        targets heroes \h -> chooseOrRunOneM pid do
          questionLabeled "desperateAlliance.choosePlayer"
          causedBy attrs
          for_ others \other -> targeting (PlayerTarget other) do
            phaseModifier attrs h (ControlReturnsAtEndOfPhase pid)
            giveHeroTo h other
      pure e
    _ -> DesperateAlliance <$> runMessage msg attrs
