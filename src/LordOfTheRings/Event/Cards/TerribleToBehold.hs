module LordOfTheRings.Event.Cards.TerribleToBehold (terribleToBehold) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Noble))

newtype TerribleToBehold = TerribleToBehold EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After you declare a Noble character as a defender against an attack made by
-- an engaged enemy, return that enemy to the staging area. Then, cancel the attack.
terribleToBehold :: EventCard TerribleToBehold
terribleToBehold =
  eventWith TerribleToBehold Cards.terribleToBehold \a ->
    let you = PlayerWithId a.owner
     in a
          & windowL
          ?~ CharacterDefenderDeclaredWindow
            After
            (HeroWithTrait Noble <> HeroControlledBy you)
            (AllyWithTrait Noble <> AllyControlledBy you)
            (EnemyEngagedWith AnyPlayer)

instance RunMessage TerribleToBehold where
  runMessage msg e@(TerribleToBehold attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (take 1 [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> do
        placeEnemy eid StagingArea
        cancelAttack eid
      pure e
    _ -> TerribleToBehold <$> runMessage msg attrs
