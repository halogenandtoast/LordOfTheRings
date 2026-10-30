module LordOfTheRings.Event.Cards.ScoutingParty (scoutingParty) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Scout))

newtype ScoutingParty = ScoutingParty EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

committedBy :: PlayerId -> (HeroMatcher, AllyMatcher)
committedBy pid = (HeroControlledBy (PlayerWithId pid) <> CommittedHero, AllyControlledBy (PlayerWithId pid) <> CommittedAlly)

-- | Response: After you commit characters to the quest, if each character you have
-- committed to the quest is a Scout, each of those characters gets +2 willpower until the
-- end of the phase.
scoutingParty :: EventCard ScoutingParty
scoutingParty =
  eventWith ScoutingParty Cards.scoutingParty \a ->
    let (heroes, allies) = committedBy a.owner
     in a
          & windowL ?~ PlayerCommittedCharactersWindow After a.owner
          & criteriaL .~ NotCriterion (CharactersAtLeast 1 (heroes <> not_ (HeroWithTrait Scout)) (allies <> not_ (AllyWithTrait Scout)))

instance RunMessage ScoutingParty where
  runMessage msg e@(ScoutingParty attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      scouts <- uncurry selectCharacters (committedBy pid)
      for_ scouts \c -> phaseModifier attrs c (WillpowerModifier 2)
      pure e
    _ -> ScoutingParty <$> runMessage msg attrs
