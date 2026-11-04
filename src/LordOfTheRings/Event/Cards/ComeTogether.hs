module LordOfTheRings.Event.Cards.ComeTogether (comeTogether) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Campaign (campaignLevel)
import LordOfTheRings.Message.Lifted.UpTo
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (traitName)

newtype ComeTogether = ComeTogether EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A scalable boon of the Dream-chaser campaign.
comeTogether :: EventCard ComeTogether
comeTogether = event ComeTogether Cards.comeTogether

{- | Action: Choose up to X characters. X is the level of Come Together, then add it to the
victory display. Until the end of the round, each of those characters...
...gets +2 defense if you control a Leadership hero.
...gets +2 attack if you control a Tactics hero.
...gets +2 willpower if you control a Spirit hero.
...gains a Trait of your choice and heals 2 damage if you control a Lore hero.
-}
instance RunMessage ComeTogether where
  runMessage msg e@(ComeTogether attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      x <- campaignLevel card
      let has sphere = selectAny (HeroControlledBy (PlayerWithId pid) <> HeroWithSphere sphere)
      leadership <- has Leadership
      tactics <- has Tactics
      spirit <- has Spirit
      lore <- has Lore
      let mods = [DefenseModifier 2 | leadership] <> [AttackModifier 2 | tactics] <> [WillpowerModifier 2 | spirit]
      characters <- selectCharacters AnyHero AnyAlly
      addToVictoryDisplay card
      chooseUpToM pid "comeTogether.choose" (toTarget attrs) [] x
        [ UpToTarget c do
            unless (null mods) $ roundModifiers attrs c mods
            when lore do
              chooseOneM pid do
                questionLabeled "comeTogether.trait"
                causedBy attrs
                for_ [minBound .. maxBound] \t ->
                  labeledWith "comeTogether.gainTrait" [textVar "trait" (traitName t)] $ roundModifier attrs c (GainTrait t)
              healDamage attrs c 2
        | c <- characters
        ]
      pure e
    _ -> ComeTogether <$> runMessage msg attrs
