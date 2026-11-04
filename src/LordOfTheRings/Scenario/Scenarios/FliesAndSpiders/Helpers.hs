{- | What Flies and Spiders' cards share: poison and unconscious characters (The Hobbit: On
the Doorstep rules insert, "Venom, Poison, and Poisoned" and "Unconscious Characters").
Poison and the venom keyword are the engine's; see 'givePoison'.
-}
module LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (
  poisonOn,
  poisonedCharacters,
  unconsciousCharacters,
  unconsciousRestrictions,
  givePoisonToCharacterOf,
  readyUnconsciousCharacterOf,
  spendToReadyUnconscious,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Ally.Types (AllyAttrs (allyPoison))
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.Types (HeroAttrs (heroPoison))
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Sphere (Sphere (Baggins))
import LordOfTheRings.Target

-- | How much poison the character has attached.
poisonOn :: HasGame m => Target -> m Int
poisonOn = \case
  HeroTarget hid -> length . heroPoison . toAttrs <$> getHero hid
  AllyTarget aid -> length . allyPoison . toAttrs <$> getAlly aid
  _ -> pure 0

-- | The characters with any poison attached.
poisonedCharacters :: HasGame m => m [Target]
poisonedCharacters = selectCharacters PoisonedHero PoisonedAlly

unconsciousCharacters :: HasGame m => m [Target]
unconsciousCharacters = selectCharacters UnconsciousHero UnconsciousAlly

{- | Each stage but 2B reads: "Unconscious characters cannot quest, attack, defend, collect
resources, trigger abilities, be poisoned, or ready (except by effects that target
unconscious characters)." Readying an unconscious character is 'readyUnconscious', which
this does not stop.
-}
unconsciousRestrictions :: (HasModifiersM m, Sourceable source) => source -> m ()
unconsciousRestrictions source = do
  characters <- unconsciousCharacters
  for_ characters \c ->
    modified source c [CannotCommitToQuest, CannotAttackOrDefend, CannotCollectResources, CannotTriggerAbilities, CannotBePoisoned, CannotReady]

-- | The player gives this much poison to a character he controls, of his choice: one that
-- can be poisoned, if able.
givePoisonToCharacterOf :: (ReverseQueue m, Targetable source) => source -> PlayerId -> Int -> m ()
givePoisonToCharacterOf source pid n = do
  characters <- charactersControlledBy pid
  able <- filterM (fmap (notElem CannotBePoisoned) . getModifiers) characters
  chooseOrRunOneM pid do
    questionLabeled "fliesAndSpiders.givePoison"
    causedBy source
    targets able \c -> givePoison c n

-- | The player readies an unconscious character he controls.
readyUnconsciousCharacterOf :: (ReverseQueue m, Targetable source) => source -> PlayerId -> m ()
readyUnconsciousCharacterOf source pid = do
  characters <- selectCharacters (UnconsciousHero <> HeroControlledBy (PlayerWithId pid)) (UnconsciousAlly <> AllyControlledBy (PlayerWithId pid))
  chooseOrRunOneM pid do
    questionLabeled "fliesAndSpiders.readyUnconscious"
    causedBy source
    targets characters readyUnconscious

-- | Stages 3B and 4B: "Action: Spend 2 Baggins resources to ready an unconscious character
-- you control." Any player may trigger it, paying from his own heroes.
spendToReadyUnconscious :: Sourceable a => a -> Int -> Ability
spendToReadyUnconscious a idx =
  anyPlayer
    $ restricted
      a
      idx
      (AnyCriteria [exists (UnconsciousHero <> HeroControlledBy ActivePlayer), exists (UnconsciousAlly <> AllyControlledBy ActivePlayer)])
    $ ActionAbility (SpendResourcesCost Baggins 2)
